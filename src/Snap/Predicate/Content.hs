{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicate.Content
  ( Content
  , ContentType (..)
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

type Content x y = MediaType x y

-- | A 'Predicate' against the 'Request's \"Content-Type\" header.
data ContentType t s = ContentType t s deriving Eq

instance (MType t, MSubType s) => Predicate (ContentType t s) Request where
    type FVal (ContentType t s) = Error
    type TVal (ContentType t s) = MediaType t s
    apply (ContentType x y) r   = do
        mtypes <- E.lookup "content-type" >>= maybe (readMediaTypes "content-type" r) return
        case mediaType False x y mtypes of
               Just m  -> return (T 0 m)
               Nothing -> return (F (err 415 message))
      where
        message = "Expected 'Content-Type: "
                    <> fromString (show x)
                    <> "/"
                    <> fromString (show y)
                    <> "'."

instance (Show t, Show s) => Show (ContentType t s) where
    show (ContentType t s) = "ContentType: " ++ show t ++ "/" ++ show s
