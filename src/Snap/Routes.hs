{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
module Snap.Routes
  ( Routes
  , showRoutes
  , expandRoutes
  , get
  , post
  , put
  , delete
  )
where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.Either
import Data.Monoid
import Data.String
import Data.Word
import Predicates
import Snap.Core
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as L

data Pack where
    Pack :: (Show p, Predicate p Request)
         => p
         -> (Value p -> Snap ())
         -> Pack

data Route = Route
  { _method  :: !Method
  , _path    :: !ByteString
  , _pred    :: !Pack
  }

-- | A monad holding a sequence of routes, which
-- are added per 'Method' via 'get', 'post' etc.
-- A route is defined by 'Method', path, 'Predicate'
-- and the actual Snap handler.
newtype Routes a = Routes
  { _unroutes :: State [Route] a }

instance Monad Routes where
    return  = Routes . return
    m >>= f = Routes $ _unroutes m >>= _unroutes . f

addRoute :: (Show p, Predicate p Request)
         => Method
         -> ByteString           -- path
         -> (Value p -> Snap ()) -- handler
         -> p                    -- predicate
         -> Routes ()
addRoute m r x p = Routes $ State.modify ((Route m r (Pack p x)):)

-- | Add a route with 'Method' 'GET'
get :: (Show p, Predicate p Request)
    => ByteString           -- ^ path
    -> (Value p -> Snap ()) -- ^ handler
    -> p                    -- ^ 'Predicate'
    -> Routes ()
get = addRoute GET

-- | Add a route with 'Method' 'POST'
post :: (Show p, Predicate p Request)
     => ByteString           -- ^ path
     -> (Value p -> Snap ()) -- ^ handler
     -> p                    -- ^ 'Predicate'
     -> Routes ()
post = addRoute POST

-- | Add a route with 'Method' 'PUT'
put :: (Show p, Predicate p Request)
    => ByteString           -- ^ path
    -> (Value p -> Snap ()) -- ^ handler
    -> p                    -- ^ 'Predicate'
    -> Routes ()
put = addRoute PUT

-- | Add a route with 'Method' 'DELETE'
delete :: (Show p, Predicate p Request)
       => ByteString           -- ^ path
       -> (Value p -> Snap ()) -- ^ handler
       -> p                    -- ^ 'Predicate'
       -> Routes ()
delete = addRoute DELETE

-- | Turn route definitions into string format.
-- Each route is represented as a 'ByteString'.
showRoutes :: Routes () -> [ByteString]
showRoutes (Routes routes) =
    let rs = reverse $ State.execState routes []
    in flip map rs $ \x ->
        case _pred x of
            Pack p _ -> show' (_method x) <> " " <> show' (_path x) <> " " <> show' p
  where
    show' :: Show a => a -> ByteString
    show' = fromString . show

-- | Turn route definitions into "snapable" format, i.e.
-- Routes are grouped per path and selection evaluates routes
-- against the given Snap 'Request'.
expandRoutes :: Routes () -> [(ByteString, Snap ())]
expandRoutes (Routes routes) =
    let rg = grouped . sorted . reverse $ State.execState routes []
    in map (\g -> (_path (head g), select g)) rg
  where
    sorted :: [Route] -> [Route]
    sorted = L.sortBy (\a b -> _path a `compare` _path b)

    grouped :: [Route] -> [[Route]]
    grouped = L.groupBy (\a b -> _path a == _path b)

-- The handler selection proceeds as follows:
-- (1) Consider only handlers with matching methods, or else return 405.
-- (2) Evaluate 'Route' predicates.
-- (3) Pick the first one which is 'Good', or else respond with status
--     and message of the first one.
select :: [Route] -> Snap ()
select g = do
    ms <- filterM byMethod g
    if L.null ms
        then respond (No 405 Nothing)
        else evalAll ms
  where
    byMethod :: MonadSnap m => Route -> m Bool
    byMethod x = (_method x ==) <$> getsRequest rqMethod

    evalAll :: [Route] -> Snap ()
    evalAll rs = do
        req <- getRequest
        let (n, y) = partitionEithers $ map (eval req) rs
        if null y
            then let (i, m) = head n in respond (No i m)
            else head y

    eval :: Request -> Route -> Either (Word, Maybe ByteString) (Snap ())
    eval rq r = case _pred r of
        Pack p h ->
            case apply p rq of
                No i m -> Left (i, m)
                Yes v  -> Right (h v)

respond :: MonadSnap m => Result a -> m ()
respond (No i msg) = do
    putResponse . clearContentLength
                . setResponseCode (fromIntegral i)
                $ emptyResponse
    maybe (return ()) writeBS msg
respond _ = return ()
