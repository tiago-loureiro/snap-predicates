{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Snap.Predicate.Internal
  ( RqPred (..)
  , headers
  , params
  , cookies
  , readValues
  , rqApply
  , rqApplyMaybe
  , key
  )
where

import Control.Monad.State.Class
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Maybe
import Data.Monoid
import Data.Predicate
import Data.Predicate.Env (Env)
import Data.String
import Data.Typeable
import Snap.Core hiding (headers)
import Snap.Predicate.Error
import Snap.Util.Readable

import qualified Data.ByteString.Char8 as C
import qualified Data.Predicate.Env    as E
import qualified Data.Map.Strict       as M

headers :: ByteString -> Request -> [ByteString]
headers name = fromMaybe [] . getHeaders (mk name)

params :: ByteString -> Request -> [ByteString]
params name = fromMaybe [] . M.lookup name . rqParams

cookies :: ByteString -> Request -> [Cookie]
cookies name rq = filter ((name ==) . cookieName) (rqCookies rq)

-- | Specialized Either monad for use with 'fromBS' within 'readValues'
-- that implements 'fail' with 'Left' in order to preserve error
-- messages produced by 'Readable' instances.
newtype EitherS a = EitherS { unEitherS :: Either ByteString a }
instance Monad EitherS where
    (EitherS e) >>= f = EitherS $ e >>= unEitherS . f
    return = EitherS . return
    fail   = EitherS . Left . C.pack

readValues :: Readable a => [ByteString] -> Either ByteString a
readValues vs = catEitherS $ map fromBS vs
  where
    catEitherS [] = Left "no read"
    catEitherS (EitherS(e):es) = e `orElse` catEitherS es
      where
        orElse (Right x) _         = Right x
        orElse _         (Right y) = Right y
        orElse (Left  x) (Left  _) = Left  x

data RqPred a = RqPred
  { _rqName      :: !ByteString
  , _rqRead      :: [ByteString] -> Either ByteString a
  , _rqDef       :: !(Maybe a)
  , _rqCachePref :: !ByteString
  , _rqVals      :: Request -> [ByteString]
  , _rqError     :: !(Maybe Error)
  }

rqApply :: (Typeable a, MonadState m, StateType m ~ Env)
        => RqPred a -> Request -> m (Boolean Error a)
rqApply p r =
    let k = key (_rqCachePref p) (_rqName p) (_rqDef p)
    in E.lookup k >>= maybe (work k) result
  where
    work k = case _rqVals p r of
        [] -> return $ maybe (F (fromMaybe defErr (_rqError p))) (T 0) (_rqDef p)
        vs -> do
            let v = _rqRead p vs
            E.insert k v
            result v

    result = return . either (F . err 400) (T 0)
    defErr = Error 400 Nothing

rqApplyMaybe :: (Typeable a, MonadState m, StateType m ~ Env)
             => RqPred a -> Request -> m (Boolean Error (Maybe a))
rqApplyMaybe p r =
    let n = Nothing :: Typeable a => Maybe a
        k = key (_rqCachePref p) (_rqName p) n
    in E.lookup k >>= maybe (work k n) result
  where
    work k n = case _rqVals p r of
        [] -> return (T 0 n)
        vs -> do
            let v = _rqRead p vs
            E.insert k v
            result v

    result = return . either (F . err 400) (T 0 . Just)

key :: (Typeable a, IsString m, Monoid m) => m -> m -> a -> m
key prefix name def = prefix <> name <> ":" <> (fromString . show . typeOf $ def)
