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
import Control.Monad.State.Strict (runStateT)
import Data.ByteString (ByteString)
import Data.Either
import Data.Predicate
import Data.Predicate.Env (Env)
import Data.Word
import Snap.Core
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as L
import qualified Data.Predicate.Env as E

type Error = (Word, Maybe ByteString)

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
addRoute m r x p = Routes $ State.modify ((Route m r (Pack p x)):)

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
    let rs = reverse $ State.execState routes []
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
    let rg = grouped . sorted . reverse $ State.execState routes []
    in map (\g -> (_path (L.head g), select g)) rg
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
        then respond (405, Nothing)
        else evalAll ms
  where
    byMethod :: MonadSnap m => Route m -> m Bool
    byMethod x = (_method x ==) <$> getsRequest rqMethod

    evalAll :: MonadSnap m => [Route m] -> m ()
    evalAll rs = do
        req <- getRequest
        let (n, y) = partitionEithers $ eval1 req E.empty rs
        if null y
            then respond (L.head n)
            else L.head y

    eval1 :: MonadSnap m => Request -> Env -> [Route m] -> [Either Error (m ())]
    eval1 _  _ []     = []
    eval1 rq e (r:rs) = let (e', x) = eval rq e r in x : eval1 rq e' rs

    eval :: MonadSnap m => Request -> Env -> Route m -> (Env, Either Error (m ()))
    eval rq e r = case _pred r of
        Pack p h ->
            case runStateT (apply p rq) e of
                F Nothing  -> (e, Left (500, Nothing))
                F (Just m) -> (e, Left m)
                T (v, e')  -> (e', Right (h v))

respond :: MonadSnap m => Error -> m ()
respond (i, msg) = do
    putResponse . clearContentLength
                . setResponseCode (fromIntegral i)
                $ emptyResponse
    maybe (return ()) writeBS msg
