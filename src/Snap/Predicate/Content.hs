{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Snap.Predicate.Content
  ( ContentType
  , contentType
  , module Snap.Predicate.MediaType
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Monoid hiding (All)
import Data.Predicate
import Data.Predicate.Descr
import GHC.TypeLits
import Data.Maybe
import Snap.Core hiding (headers)
import Snap.Predicate.Error
import Snap.Predicate.MediaType
import Snap.Predicate.MediaType.Internal

import qualified Data.Predicate.Env           as E
import qualified Snap.Predicate.Parser.Accept as A

-- | A 'Predicate' against the 'Request's \"Content-Type\" header.
data ContentType (t :: Symbol) (s :: Symbol) = ContentType

{-# INLINE contentType #-}
contentType :: ContentType t s
contentType = ContentType

type1 :: SingI t => ContentType t s -> ByteString
type1 m = withSing (f m)
  where
    f :: ContentType t s -> Sing t -> ByteString
    f _ t = pack $ fromSing t

type2 :: SingI s => ContentType t s -> ByteString
type2 m = withSing (f m)
  where
    f :: ContentType t s -> Sing s -> ByteString
    f _ s = pack $ fromSing s

instance (SingI t, SingI s) => Predicate (ContentType t s) Request where
    type FVal (ContentType t s) = Error
    type TVal (ContentType t s) = Media t s
    apply c r = do
        mtypes <- E.lookup "content-type" >>= maybe (readMediaTypes "content-type" r) return
        case findContentType c mtypes of
               m:_ -> return (T (1.0 - mediaQuality m) m)
               []  -> return (F (err 415 msg))
      where
        msg = "Expected 'Content-Type: " <> type1 c <> "/" <> type2 c <> "'."

instance (SingI t, SingI s) => Show (ContentType t s) where
    show c = unpack $ "ContentType: " <> type1 c <> "/" <> type2 c

instance (SingI t, SingI s) => Description (ContentType t s) where
    describe a = DSymbol "Content-Type" (show $ type1 a <> "/" <> type2 a) Required

findContentType :: (SingI t, SingI s) => ContentType t s -> [A.MediaType] -> [Media t s]
findContentType c = mapMaybe (\m -> do
    let ct = type1 c
        cs = type2 c
        mt = A.medType m
        ms = A.medSubtype m
    guard (ct == "*" || ct == mt && cs == "*" || cs == ms)
    return $ Media mt ms (quality ct cs) (A.medParams m))
  where
    quality "*" "*" = 0
    quality "*"  _  = 0.2
    quality  _  "*" = 0.5
    quality  _   _  = 1.0
