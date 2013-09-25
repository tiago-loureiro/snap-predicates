{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Snap.Predicate.Accept
  ( Accept
  , accept
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

-- | A 'Predicate' against the 'Request's \"Accept\" header.
data Accept (t :: Symbol) (s :: Symbol) = Accept

{-# INLINE accept #-}
accept :: Accept t s
accept = Accept

type1 :: SingI t => Accept t s -> ByteString
type1 m = withSing (f m)
  where
    f :: Accept t s -> Sing t -> ByteString
    f _ t = pack $ fromSing t

type2 :: SingI s => Accept t s -> ByteString
type2 m = withSing (f m)
  where
    f :: Accept t s -> Sing s -> ByteString
    f _ s = pack $ fromSing s


instance (SingI t, SingI s) => Predicate (Accept t s) Request where
    type FVal (Accept t s) = Error
    type TVal (Accept t s) = Media t s
    apply a r = do
        mtypes <- E.lookup "accept" >>= maybe (readMediaTypes "accept" r) return
        if null mtypes
            then return (T 0 (Media (type1 a) (type2 a) 1.0 []))
            else case findMediaType a mtypes of
               m:_ -> return (T (1.0 - mediaQuality m) m)
               []  -> return (F (err 406 msg))
      where
        msg = "Expected 'Accept: " <> type1 a <> "/" <> type2 a <> "'."

instance (SingI t, SingI s) => Show (Accept t s) where
    show a = unpack $ "Accept: " <> type1 a <> "/" <> type2 a

instance (SingI t, SingI s) => Description (Accept t s) where
    describe a = DSymbol "Accept" (show $ type1 a <> "/" <> type2 a) Optional ["Header"]

findMediaType :: (SingI t, SingI s) => Accept t s -> [A.MediaType] -> [Media t s]
findMediaType a = mapMaybe (\m -> do
    let at = type1 a
        as = type2 a
        mt = A.medType m
        ms = A.medSubtype m
    guard (mt == "*" || at == mt && ms == "*" || as == ms)
    return $ Media at as (A.medQuality m) (A.medParams m))
