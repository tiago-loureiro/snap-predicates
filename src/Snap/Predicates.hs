{-# LANGUAGE OverloadedStrings, BangPatterns, TypeOperators, TupleSections #-}
module Snap.Predicates
  ( Predicate (..)
  , Routes
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
import Data.Monoid
import Data.String
import Data.Word
import Monad.Helpers
import Snap.Core
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Map.Strict as M

data Route m = Route
  { _method  :: !Method
  , _path    :: !ByteString
  , _pred    :: !Predicate
  , _handler :: m ()
  }

newtype Routes m a = Routes
  { _unroutes :: State [Route m] a }

instance Monad (Routes m) where
    return  = Routes . return
    m >>= f = Routes $ _unroutes m >>= _unroutes . f

addRoute :: MonadSnap m => Method -> ByteString -> m () -> Predicate -> Routes m ()
addRoute m r x p = Routes $ State.modify ((Route m r p x):)

get, post, put, delete :: MonadSnap m => ByteString -> m () -> Predicate -> Routes m ()
get    = addRoute GET
post   = addRoute POST
put    = addRoute PUT
delete = addRoute DELETE

showRoutes :: Routes m () -> [ByteString]
showRoutes (Routes routes) =
    let rs = reverse $ State.execState routes []
    in flip map rs $ \x ->
        show' (_method x) <> " " <> show' (_path x) <> " " <> show' (_pred x)
  where
    show' :: Show a => a -> ByteString
    show' = fromString . show

expandRoutes :: MonadSnap m => Routes m () -> [(ByteString, m ())]
expandRoutes (Routes routes) =
    let rg = grouped . sorted . reverse $ State.execState routes []
    in map (\g -> (_path (head g), select g)) rg
  where
    sorted :: [Route m] -> [Route m]
    sorted = L.sortBy (\a b -> _path a `compare` _path b)

    grouped :: [Route m] -> [[Route m]]
    grouped = L.groupBy (\a b -> _path a == _path b)

select :: MonadSnap m => [Route m] -> m ()
select g = do
    ms <- filterM byMethod g
    if L.null ms
        then respond (Bad 405 Nothing)
        else evalPreds ms
  where
    byMethod :: MonadSnap m => Route m -> m Bool
    byMethod x = (_method x ==) <$> getsRequest rqMethod

    evalPreds :: MonadSnap m => [Route m] -> m ()
    evalPreds rs = do
        es <- forM rs $ \r -> (, _handler r) <$> eval (_pred r)
        case L.find ((== Good) . fst) es of
            Just (_, h) -> h
            Nothing     -> respond (fst . head $ es)

respond :: MonadSnap m => Result -> m ()
respond (Bad i msg) = do
    putResponse . clearContentLength
                . setResponseCode (fromIntegral i)
                $ emptyResponse
    maybe (return ()) writeBS msg
respond Good = return ()

-- Predicates

data Result =
    Good
  | Bad !Word !(Maybe ByteString)
  deriving (Eq, Show)

data Predicate =
    Accept ByteString
  | AnyParamOf [ByteString]
  | AnyHeaderOf [ByteString]
  | Predicate :&: Predicate
  | Predicate :|: Predicate
  deriving (Eq, Show)

eval :: MonadSnap m => Predicate -> m Result
eval (Accept x) =
    unlessM (elem x <$> headers' "accept") $
        Bad 406 (Just "Expected 'Accept: accept/json'.")

eval (AnyParamOf xs) =
    whenM (null . concat <$> mapM params' xs) $
        Bad 400 (Just ("Expected any of " <> (fromString . show $ xs) <> "."))

eval (AnyHeaderOf xs) =
    whenM (null . concat <$> mapM headers' xs) $
        Bad 400 (Just ("Expected any of " <> (fromString . show $ xs) <> "."))

eval (x :&: y) = eval x >>= \b ->
    case b of
        Good -> eval y
        bad  -> return bad

eval (x :|: y) = eval x >>= \b ->
    case b of
        Good -> return Good
        _    -> eval y

whenM, unlessM :: (Monad m, Functor m) => m Bool -> Result -> m Result
whenM   t x = ifM t x Good
unlessM t x = ifM t Good x

headers' :: MonadSnap m => ByteString -> m [ByteString]
headers' name = maybe [] id . getHeaders (CI.mk name) <$> getRequest

params' :: MonadSnap m => ByteString -> m [ByteString]
params' name = maybe [] id . M.lookup name <$> getsRequest rqParams
