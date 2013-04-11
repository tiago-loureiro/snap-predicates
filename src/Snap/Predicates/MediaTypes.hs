{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns          #-}
module Snap.Predicates.MediaTypes
  ( Type (..)
  , SubType (..)
  , MediaType (..)

  -- * Predicate
  , Accept (..)

  -- * Media-Types
  , Typ (..)
  , All (..)
  , Application (..)

  -- * Media-Sub-Types
  , SubTyp (..)
  , Json (..)
  , Thrift (..)
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid hiding (All)
import Data.String
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicates
import Snap.Predicates.Internal
import qualified Data.Predicate.Env as E
import qualified Snap.Predicates.Parsers.Accept as A

class (Show a, Eq a) => Type a where
    toType :: a -> ByteString -> Maybe a

class (Show a, Eq a) => SubType a where
    toSubType :: a -> ByteString -> Maybe a

data MediaType t s = MediaType
  { _type    :: !t
  , _subtype :: !s
  , _quality :: !Double
  , _params  :: ![(ByteString, ByteString)]
  } deriving (Eq, Show)

-- | A 'Predicate' against the 'Request's "Accept" header.
data Accept t s = Accept t s deriving Eq

instance (Type t, SubType s) => Predicate (Accept t s) Request where
    type FVal (Accept t s) = Error
    type TVal (Accept t s) = MediaType t s
    apply (Accept x y) r = do
        mtypes <- E.lookup "accept" >>= maybe readMediaTypes return
        case mediaType x y mtypes of
               Just m  -> return (T m)
               Nothing -> return (F (err 406 message))
      where
        q a b = A.medQuality b `compare` A.medQuality a

        readMediaTypes = do
            let mtypes = sortBy q . concat . map A.parseMediaTypes $ headers r "accept"
            E.insert "accept" mtypes
            return mtypes

        message = "Expected 'Accept: "
                    <> fromString (show x)
                    <> "/"
                    <> fromString (show y)
                    <> "'."

instance (Show t, Show s) => Show (Accept t s) where
    show (Accept t s) = "Accept: " ++ show t ++ "/" ++ show s

mediaType :: (Type t, SubType s) => t -> s -> [A.MediaType] -> Maybe (MediaType t s)
mediaType t s = head' . mapMaybe (\m -> do
    t' <- if A.medType    m == "*" then Just t else toType t    (A.medType m)
    s' <- if A.medSubtype m == "*" then Just s else toSubType s (A.medSubtype m)
    guard (t == t' && s == s')
    return $ MediaType t s (A.medQuality m) (A.medParams m))
  where
    head' []    = Nothing
    head' (h:_) = Just h

-- | Generic media-type.
data Typ = Typ ByteString deriving Eq

instance Type Typ where
    toType o@(Typ t) s = if t == s then Just o else Nothing

instance Show Typ where
    show (Typ t) = show t

-- | Generic media-subtype.
data SubTyp = SubTyp ByteString deriving Eq

instance SubType SubTyp where
    toSubType o@(SubTyp t) s = if t == s then Just o else Nothing

instance Show SubTyp where
    show (SubTyp t) = show t

-- | media-type and sub-type \"*\".
data All = All deriving Eq

instance Type All where
    toType _ s = if s == "*" then Just All else Nothing

instance SubType All where
    toSubType _ s = if s == "*" then Just All else Nothing

instance Show All where
    show _ = "*"

-- | media-type \"application\".
data Application = Application deriving Eq

instance Type Application where
    toType _ s = if s == "application" then Just Application else Nothing

instance Show Application where
    show _ = "application"

-- | media-subtype \"json\".
data Json = Json deriving Eq

instance SubType Json where
    toSubType _ s = if s == "json" then Just Json else Nothing

instance Show Json where
    show _ = "json"

-- | media-subtype \"x-thrift\".
data Thrift = Thrift deriving Eq

instance SubType Thrift where
    toSubType _ s = if s == "x-thrift" then Just Thrift else Nothing

instance Show Thrift where
    show _ = "x-thrift"
