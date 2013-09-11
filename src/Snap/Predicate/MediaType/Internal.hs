{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Snap.Predicate.MediaType.Internal where

import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.List (sortBy)
import Data.Maybe
import Snap.Core (Request)
import Snap.Predicate.Internal
import Snap.Predicate.MediaType

import qualified Data.Predicate.Env           as E
import qualified Snap.Predicate.Parser.Accept as A

mediaType :: (MType t, MSubType s) => t -> s -> [A.MediaType] -> Maybe (MediaType t s)
mediaType t s = safeHead . mapMaybe (\m -> do
    t' <- if A.medType    m == "*" then Just t else toType    (A.medType m)
    s' <- if A.medSubtype m == "*" then Just s else toSubType (A.medSubtype m)
    guard (t == t' && s == s')
    return $ MediaType t s (A.medQuality m) (A.medParams m))

contentType :: (MType t, MSubType s) => t -> s -> [A.MediaType] -> Maybe (MediaType t s)
contentType t s = safeHead . mapMaybe (\m -> do
    let st = show t
        ss = show s
        mt = unpack (A.medType m)
        ms = unpack (A.medSubtype m)
    guard (st == "*" || st == mt && ss == "*" || ss == ms)
    return $ MediaType t s (quality st ss) (A.medParams m))
  where
    quality "*" "*" = 0
    quality "*"  _  = 0.2
    quality  _  "*" = 0.5
    quality  _   _  = 1.0

readMediaTypes :: (MonadState m, StateType m ~ E.Env) => ByteString -> Request -> m [A.MediaType]
readMediaTypes k r = do
    let mtypes = sortBy q . concatMap A.parseMediaTypes $ headers k r
    E.insert k mtypes
    return mtypes
  where
    q a b = A.medQuality b `compare` A.medQuality a
