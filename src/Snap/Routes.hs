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
import Predicates hiding (True, False)
import Snap.Core
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as L

data Pred where
    Pred :: (Show p, Predicate p Request) => p -> Pred

data Route m = Route
  { _method  :: !Method
  , _path    :: !ByteString
  , _pred    :: !Pred
  , _handler :: m ()
  }

-- | A monad holding a sequence of routes, which
-- are added per 'Method' via 'get', 'post' etc.
-- A route is defined by 'Method', path, 'Predicate'
-- and the actual Snap handler.
newtype Routes m a = Routes
  { _unroutes :: State [Route m] a }

instance Monad (Routes m) where
    return  = Routes . return
    m >>= f = Routes $ _unroutes m >>= _unroutes . f

addRoute :: (MonadSnap m, Show p, Predicate p Request)
         => Method
         -> ByteString -- path
         -> m ()       -- handler
         -> p          -- predicate
         -> Routes m ()
addRoute m r x p = Routes $ State.modify ((Route m r (Pred p) x):)

-- | Add a route with 'Method' 'GET'
get :: (MonadSnap m, Show p, Predicate p Request)
    => ByteString -- ^ path
    -> m ()       -- ^ handler
    -> p          -- ^ 'Predicate'
    -> Routes m ()
get = addRoute GET

-- | Add a route with 'Method' 'POST'
post :: (MonadSnap m, Show p, Predicate p Request)
     => ByteString -- ^ path
     -> m ()       -- ^ handler
     -> p          -- ^ 'Predicate'
     -> Routes m ()
post = addRoute POST

-- | Add a route with 'Method' 'PUT'
put :: (MonadSnap m, Show p, Predicate p Request)
    => ByteString -- ^ path
    -> m ()       -- ^ handler
    -> p          -- ^ 'Predicate'
    -> Routes m ()
put = addRoute PUT

-- | Add a route with 'Method' 'DELETE'
delete :: (MonadSnap m, Show p, Predicate p Request)
       => ByteString -- ^ path
       -> m ()       -- ^ handler
       -> p          -- ^ 'Predicate'
       -> Routes m ()
delete = addRoute DELETE

-- | Turn route definitions into string format.
-- Each route is represented as a 'ByteString'.
showRoutes :: Routes m () -> [ByteString]
showRoutes (Routes routes) =
    let rs = reverse $ State.execState routes []
    in flip map rs $ \x ->
        case _pred x of
            Pred p -> show' (_method x) <> " " <> show' (_path x) <> " " <> show' p
  where
    show' :: Show a => a -> ByteString
    show' = fromString . show

-- | Turn route definitions into "snapable" format, i.e.
-- Routes are grouped per path and selection evaluates routes
-- against the given Snap 'Request'.
expandRoutes :: MonadSnap m => Routes m () -> [(ByteString, m ())]
expandRoutes (Routes routes) =
    let rg = grouped . sorted . reverse $ State.execState routes []
    in map (\g -> (_path (head g), select g)) rg
  where
    sorted :: [Route m] -> [Route m]
    sorted = L.sortBy (\a b -> _path a `compare` _path b)

    grouped :: [Route m] -> [[Route m]]
    grouped = L.groupBy (\a b -> _path a == _path b)

-- The handler selection proceeds as follows:
-- (1) Consider only handlers with matching methods, or else return 405.
-- (2) Evaluate 'Route' predicates.
-- (3) Pick the first one which is 'Good', or else respond with status
--     and message of the first one.
select :: MonadSnap m => [Route m] -> m ()
select g = do
    ms <- filterM byMethod g
    if L.null ms
        then respond (No 405 Nothing)
        else evalAll ms
  where
    byMethod :: MonadSnap m => Route m -> m Bool
    byMethod x = (_method x ==) <$> getsRequest rqMethod

    evalAll :: MonadSnap m => [Route m] -> m ()
    evalAll rs = do
        req <- getRequest
        let (n, y) = partitionEithers $ map (eval req) rs
        if null y
            then let (i, m) = head n in respond (No i m)
            else head y

    eval :: MonadSnap m => Request -> Route m -> Either (Word, Maybe ByteString) (m ())
    eval rq r = case _pred r of
        Pred p ->
            case apply p rq of
                No i m -> Left (i, m)
                Yes v  -> Right (_handler r)

respond :: MonadSnap m => Result a -> m ()
respond (No i msg) = do
    putResponse . clearContentLength
                . setResponseCode (fromIntegral i)
                $ emptyResponse
    maybe (return ()) writeBS msg
respond _ = return ()
