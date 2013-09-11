{-# LANGUAGE TypeFamilies #-}

module Snap.Predicate.MediaType.Internal where

import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.List (sortBy)
import Snap.Core (Request)
import Snap.Predicate.Internal

import qualified Data.Predicate.Env           as E
import qualified Snap.Predicate.Parser.Accept as A

readMediaTypes :: (MonadState m, StateType m ~ E.Env) => ByteString -> Request -> m [A.MediaType]
readMediaTypes k r = do
    let mtypes = sortBy q . concatMap A.parseMediaTypes $ headers k r
    E.insert k mtypes
    return mtypes
  where
    q a b = A.medQuality b `compare` A.medQuality a
