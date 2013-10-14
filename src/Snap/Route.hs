{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
module Snap.Route
  ( Routes
  , RouteDescr (..)
  , showRoutes
  , describeRoutes
  , expandRoutes
  , renderErrorWith
  , addRoute
  , get
  , get_
  , Snap.Route.head
  , head_
  , post
  , post_
  , put
  , put_
  , delete
  , delete_
  , trace
  , trace_
  , options
  , options_
  , connect
  , connect_
  )
where

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.State.Strict hiding (get, put)
import Data.ByteString (ByteString)
import Data.Either
import Data.Function
import Data.List hiding (head, delete)
import Data.Predicate
import Data.Predicate.Descr
import Snap.Core
import Snap.Predicate

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.List             as L

data Pack m where
    Pack :: (Description p, Show p, Predicate p Request, FVal p ~ Error)
         => p
         -> (TVal p -> m ())
         -> Pack m

data Route m = Route
  { _method  :: !Method
  , _path    :: !ByteString
  , _pred    :: !(Pack m)
  }

data RouteDescr = RouteDescr
    { routeMethod  :: !Method
    , routePath    :: !ByteString
    , routeDescr   :: !Descr
    } deriving Show

-- | Function to turn an 'Error' value into a 'Lazy.ByteString'.
-- Clients can provide their own renderer using 'renderErrorWith'.
type Renderer = Error -> Maybe Lazy.ByteString

-- | The Routes monad state type.
data St m = St ![Route m] !Renderer

-- | Initial state.
iniSt :: St m
iniSt = St [] (fmap Lazy.fromStrict . message)

-- | The Routes monad is used to add routing declarations via 'addRoute' or
-- one of 'get', 'post', etc.
-- Routing declarations can then be turned into the ordinary snap format,
-- i.e. @MonadSnap m => [(ByteString, m a)]@ or into strings.
newtype Routes m a = Routes
  { _unroutes :: State (St m) a }

instance Monad (Routes m) where
    return  = Routes . return
    m >>= f = Routes $ _unroutes m >>= _unroutes . f

-- | Add a route for some 'Method' and path (potentially with variable
-- captures), and constrained the some 'Predicate'.
addRoute :: (MonadSnap m, Description p, Show p, Predicate p Request, FVal p ~ Error)
         => Method
         -> ByteString        -- ^ path
         -> (TVal p -> m ())  -- ^ handler
         -> p                 -- ^ 'Predicate'
         -> Routes m ()
addRoute m r x p = Routes . modify $ \(St !rr !f) ->
    St (Route m r (Pack p x) : rr) f

renderErrorWith :: Monad m => Renderer -> Routes m ()
renderErrorWith f = Routes . modify $ \(St !rr _) -> St rr f

-- | Specialisation of 'addRoute' for a specific HTTP 'Method'.
get, head, post, put, delete, trace, options, connect ::
    (MonadSnap m, Description p, Show p, Predicate p Request, FVal p ~ Error)
    => ByteString        -- ^ path
    -> (TVal p -> m ())  -- ^ handler
    -> p                 -- ^ 'Predicate'
    -> Routes m ()
get     = addRoute GET
head    = addRoute HEAD
post    = addRoute POST
put     = addRoute PUT
delete  = addRoute DELETE
trace   = addRoute TRACE
options = addRoute OPTIONS
connect = addRoute CONNECT

-- | Specialisation of 'addRoute' for a specific HTTP 'Method' taking
-- no 'Predicate' into consideration.
get_, head_, post_, put_, delete_, trace_, options_, connect_ ::
    (MonadSnap m)
    => ByteString    -- ^ path
    -> (() -> m ())  -- ^ handler
    -> Routes m ()
get_     p h = addRoute GET     p h (Const ())
head_    p h = addRoute HEAD    p h (Const ())
post_    p h = addRoute POST    p h (Const ())
put_     p h = addRoute PUT     p h (Const ())
delete_  p h = addRoute DELETE  p h (Const ())
trace_   p h = addRoute TRACE   p h (Const ())
options_ p h = addRoute OPTIONS p h (Const ())
connect_ p h = addRoute CONNECT p h (Const ())

-- | Turn route definitions into a list of 'String's.
showRoutes :: Routes m () -> [String]
showRoutes (Routes routes) =
    let St rr _ = execState routes iniSt
    in flip map (concat (normalise rr)) $ \x ->
        case _pred x of
            Pack p _ -> shows (_method x)
                      . (' ':)
                      . shows (_path x)
                      . (' ':)
                      . shows p $ ""

-- | Turn route definitions into corresponding descriptions, i.e.
-- 'RouteDescr' values which contain the description AST.
describeRoutes :: Routes m () -> [RouteDescr]
describeRoutes (Routes routes) =
    let St rr _ = execState routes iniSt
    in flip map (concat (normalise rr)) $ \x ->
        case _pred x of Pack p _ -> RouteDescr (_method x) (_path x) (describe p)

-- | Turn route definitions into \"snapable\" format, i.e.
-- Routes are grouped per path and selection evaluates routes
-- against the given Snap 'Request'.
expandRoutes :: MonadSnap m => Routes m () -> [(ByteString, m ())]
expandRoutes (Routes routes) =
    let St rr f = execState routes iniSt in
    map (\g -> (_path (L.head g), select f g)) (normalise rr)

-- | Group routes by path.
normalise :: [Route m] -> [[Route m]]
normalise rr =
    let rg    = grouped . sorted $ rr
        paths = map (namelessPath . L.head) rg
        ambig = paths \\ nub paths
    in if null ambig then rg else error (ambiguityMessage ambig)
  where
    sorted :: [Route m] -> [Route m]
    sorted = sortBy (compare `on` _path)

    grouped :: [Route m] -> [[Route m]]
    grouped = groupBy ((==) `on` _path)

    namelessPath :: Route m -> ByteString
    namelessPath =
        let colon = 0x3A
            slash = 0x2F
            fun s = if s /= "" && S.head s == colon then "<>" else s
        in S.intercalate "/" . map fun . S.split slash . _path

    ambiguityMessage a =
        "Paths differing only in variable names are not supported.\n"  ++
        "Problematic paths (with variable positions denoted by <>):\n" ++
        show a

data Handler m = Handler
  { _delta   :: !Delta
  , _handler :: !(m ())
  }

-- The handler selection proceeds as follows:
-- (1) Consider only handlers with matching methods, or else return 405.
-- (2) Evaluate 'Route' predicates.
-- (3) Pick the first one which is 'Good', or else respond with status
--     and message of the first one.
select :: MonadSnap m => Renderer -> [Route m] -> m ()
select f g = do
    ms <- filterM byMethod g
    if null ms
        then do
            respond f (Error 405 Nothing)
            modifyResponse (setHeader "Allow" validMethods)
        else evalAll ms
  where
    byMethod :: MonadSnap m => Route m -> m Bool
    byMethod x = (_method x ==) <$> getsRequest rqMethod

    validMethods :: ByteString
    validMethods = S.intercalate "," $ nub (C.pack . show . _method <$> g)

    evalAll :: MonadSnap m => [Route m] -> m ()
    evalAll rs = do
        req <- getRequest
        let (n, y) = partitionEithers $ foldl' (evalSingle req) [] rs
        if null y
            then respond f (L.head n)
            else closest y

    evalSingle :: MonadSnap m => Request -> [Either Error (Handler m)] -> Route m -> [Either Error (Handler m)]
    evalSingle rq rs r =
        case _pred r of
            Pack p h -> case apply p rq of
                F   m -> Left m : rs
                T d v -> Right (Handler d (h v)) : rs

    closest :: MonadSnap m => [Handler m] -> m ()
    closest = foldl' (<|>) pass
            . map _handler
            . sortBy (compare `on` _delta)

respond :: MonadSnap m => Renderer -> Error -> m ()
respond f e = do
    putResponse . clearContentLength
                . setResponseCode (fromIntegral . status $ e)
                $ emptyResponse
    maybe (return ()) writeLBS (f e)
