{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicate.Accept
  ( Accept (..)
  , module Snap.Predicate.MediaType
  )
where

import Data.Monoid hiding (All)
import Data.String
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicate.Error
import Snap.Predicate.MediaType
import Snap.Predicate.MediaType.Internal
import qualified Data.Predicate.Env as E

-- | A 'Predicate' against the 'Request's \"Accept\" header.
data Accept t s = Accept t s deriving Eq

instance (MType t, MSubType s) => Predicate (Accept t s) Request where
    type FVal (Accept t s) = Error
    type TVal (Accept t s) = MediaType t s
    apply (Accept x y) r   = do
        mtypes <- E.lookup "accept" >>= maybe (readMediaTypes "accept" r) return
        if null mtypes
            then return (T 0 (MediaType x y 1.0 []))
            else case mediaType x y mtypes of
               Just m  -> return (T (1.0 - _quality m) m)
               Nothing -> return (F (err 406 message))
      where
        message = "Expected 'Accept: "
                    <> fromString (show x)
                    <> "/"
                    <> fromString (show y)
                    <> "'."

instance (Show t, Show s) => Show (Accept t s) where
    show (Accept t s) = "Accept: " ++ show t ++ "/" ++ show s
