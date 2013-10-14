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
    ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Maybe
import Data.List (foldl')
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicate.Error
import Snap.Util.Readable

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict       as M

headers :: ByteString -> Request -> [ByteString]
headers name = fromMaybe [] . getHeaders (mk name)

params :: ByteString -> Request -> [ByteString]
params name = fromMaybe [] . M.lookup name . rqParams

cookies :: ByteString -> Request -> [Cookie]
cookies name rq = filter ((name ==) . cookieName) (rqCookies rq)

newtype Result a = Result
    { toEither :: Either ByteString a }

instance Functor Result where
    fmap = liftM

instance Monad Result where
    return  = Result . Right
    fail    = Result . Left . C.pack
    r >>= k = Result $ toEither r >>= toEither . k

readValues :: Readable a => [ByteString] -> Either ByteString a
readValues = foldl' orElse (Left "no read") . map fromBS
  where
    orElse r@(Right _) _          = r
    orElse _           (Result e) = e

data RqPred a = RqPred
  { _rqName      :: !ByteString
  , _rqRead      :: [ByteString] -> Either ByteString a
  , _rqDef       :: !(Maybe a)
  , _rqCachePref :: !ByteString
  , _rqVals      :: Request -> [ByteString]
  , _rqError     :: !(Maybe Error)
  }

rqApply :: RqPred a -> Request -> Boolean Error a
rqApply p r = case _rqVals p r of
    [] -> maybe (F (fromMaybe defErr (_rqError p))) (T 0) (_rqDef p)
    vs -> either (F . err 400) (T 0) $ _rqRead p vs
  where
    defErr = Error 400 Nothing

rqApplyMaybe :: RqPred a -> Request -> Boolean Error (Maybe a)
rqApplyMaybe p r = case _rqVals p r of
    [] -> T 0 Nothing
    vs -> either (F . err 400) (T 0 . Just) $ _rqRead p vs
