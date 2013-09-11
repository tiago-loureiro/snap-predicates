{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Snap.Predicate.MediaType where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import GHC.TypeLits

data Media (t :: Symbol) (s :: Symbol) = Media
    { rawType      :: !ByteString
    , rawSubTypes  :: !ByteString
    , mediaQuality :: !Double
    , mediaParams  :: ![(ByteString, ByteString)]
    } deriving (Eq, Show)

mediaType :: SingI t => Media t s -> ByteString
mediaType m = withSing (f m)
  where
    f :: Media t s -> Sing t -> ByteString
    f _ t = pack (fromSing t)

mediaSubType :: SingI s => Media t s -> ByteString
mediaSubType m = withSing (f m)
  where
    f :: Media t s -> Sing s -> ByteString
    f _ s = pack (fromSing s)
