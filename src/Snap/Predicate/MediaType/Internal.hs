{-# LANGUAGE TypeFamilies #-}

module Snap.Predicate.MediaType.Internal where

import Data.ByteString (ByteString)
import Data.List (sortBy)
import Snap.Core (Request)
import Snap.Predicate.Internal

import qualified Snap.Predicate.Parser.Accept as A

readMediaTypes :: ByteString -> Request -> [A.MediaType]
readMediaTypes k r = sortBy q . concatMap A.parseMediaTypes $ headers k r
  where
    q a b = A.medQuality b `compare` A.medQuality a
