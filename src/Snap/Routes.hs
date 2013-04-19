{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
module Snap.Routes
  ( Routes
  , showRoutes
  , expandRoutes
  , get
  , Snap.Routes.head
  , addRoute
  , post
  , put
  , delete
  , trace
  , options
  , connect
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict hiding (get, put)
import Data.ByteString (ByteString)
import Data.Either
import Data.List hiding (head, delete)
import Data.Predicate
import Data.Predicate.Env (Env)
import Snap.Core
import Snap.Predicates
import qualified Data.List as L
import qualified Data.Predicate.Env as E

data Pack m where
    Pack :: (Show p, Predicate p Request, FVal p ~ Error)
         => p
         -> (TVal p -> m ())
         -> Pack m

data Route m = Route
  { _method  :: !Method
  , _path    :: !ByteString
  , _pred    :: !(Pack m)
  }

newtype Routes m a = Routes
  { _unroutes :: State [Route m] a }

instance Monad (Routes m) where
    return  = Routes . return
    m >>= f = Routes $ _unroutes m >>= _unroutes . f

addRoute :: (MonadSnap m, Show p, Predicate p Request, FVal p ~ Error)
         => Method
         -> ByteString        -- ^ path
         -> (TVal p -> m ())  -- ^ handler
         -> p                 -- ^ predicate
         -> Routes m ()
addRoute m r x p = Routes $ modify ((Route m r (Pack p x)):)

get, head, post, put, delete, trace, options, connect ::
    (MonadSnap m, Show p, Predicate p Request, FVal p ~ Error)
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

-- | Turn route definitions into a list of 'String's.
showRoutes :: Routes m () -> [String]
showRoutes (Routes routes) =
    let rs = reverse $ execState routes []
    in flip map rs $ \x ->
        case _pred x of
            Pack p _ -> shows (_method x)
                      . (' ':)
                      . shows (_path x)
                      . (' ':)
                      . shows p $ ""

-- | Turn route definitions into "snapable" format, i.e.
-- Routes are grouped per path and selection evaluates routes
-- against the given Snap 'Request'.
expandRoutes :: MonadSnap m => Routes m () -> [(ByteString, m ())]
expandRoutes (Routes routes) =
    let rg = grouped . sorted $ execState routes []
    in map (\g -> (_path (L.head g), select g)) rg
  where
    sorted :: [Route m] -> [Route m]
    sorted = sortBy (\a b -> _path a `compare` _path b)

    grouped :: [Route m] -> [[Route m]]
    grouped = groupBy (\a b -> _path a == _path b)

data Handler m = Handler
  { _delta   :: !Delta
  , _handler :: !(m ())
  }

-- The handler selection proceeds as follows:
-- (1) Consider only handlers with matching methods, or else return 405.
-- (2) Evaluate 'Route' predicates.
-- (3) Pick the first one which is 'Good', or else respond with status
--     and message of the first one.
select :: MonadSnap m => [Route m] -> m ()
select g = do
    ms <- filterM byMethod g
    if null ms
        then respond (Error 405 Nothing)
        else evalAll ms
  where
    byMethod :: MonadSnap m => Route m -> m Bool
    byMethod x = (_method x ==) <$> getsRequest rqMethod

    evalAll :: MonadSnap m => [Route m] -> m ()
    evalAll rs = do
        req <- getRequest
        let (n, y) = partitionEithers . snd $ foldl' (evalSingle req) (E.empty, []) rs
        if null y
            then respond (L.head n)
            else closest y

    evalSingle :: MonadSnap m => Request -> (Env, [Either Error (Handler m)]) -> Route m -> (Env, [Either Error (Handler m)])
    evalSingle rq (e, rs) r =
        case _pred r of
            Pack p h ->
                case runState (apply p rq) e of
                    (F   m, e') -> (e', Left m : rs)
                    (T d v, e') -> (e', Right (Handler d (h v)) : rs)

    closest :: [Handler m] -> m ()
    closest xs =
        let len = maximum . map (length . _delta) $ xs
            xs' = map (\(Handler d m) -> Handler (stretch d len) m) xs
            ord = sortBy (\a b -> sum (_delta a) `compare` sum (_delta b)) xs'
        in _handler (L.head ord)

    stretch :: [Double] -> Int -> [Double]
    stretch ds i = ds ++ (take (i - length ds) [1.0, 1.0 ..])

respond :: MonadSnap m => Error -> m ()
respond e = do
    putResponse . clearContentLength
                . setResponseCode (fromIntegral . _status $ e)
                $ emptyResponse
    maybe (return ()) writeBS (_message e)
