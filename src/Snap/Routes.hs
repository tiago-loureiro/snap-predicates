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
import Data.ByteString (ByteString)
import Data.Either
import Data.Monoid
import Data.Predicate
import Data.String
import Data.Word
import Snap.Core
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as L

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

-- | Turn route definitions into string-like format.
-- Each route is represented as a 'ByteString'.
showRoutes :: Routes m () -> [ByteString]
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
        let (n, y) = partitionEithers $ map (eval req) rs
        if null y
            then respond (L.head n)
            else L.head y

    eval :: MonadSnap m => Request -> Route m -> Either Error (m ())
    eval rq r = case _pred r of
        Pack p h ->
            case apply p rq of
                F Nothing  -> Left (500, Nothing)
                F (Just m) -> Left m
                T v        -> Right (h v)

respond :: MonadSnap m => Error -> m ()
respond (i, msg) = do
    putResponse . clearContentLength
                . setResponseCode (fromIntegral i)
                $ emptyResponse
    maybe (return ()) writeBS msg
