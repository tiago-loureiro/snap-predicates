{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TemplateHaskell       #-}
module Snap.Predicates.MediaTypes
  ( -- * Types
    MType (..)
  , MSubType (..)
  , MediaType (..)

  -- * Predicate
  , Accept (..)

  -- * Media-Types
  , Type (..)
  , All (..)
  , Application (..)
  , Audio (..)
  , Image (..)
  , Message (..)
  , Multipart (..)
  , Text (..)
  , Video (..)

  -- * Media-Sub-Types
  , SubType (..)
  , AtomXml (..)
  , Css (..)
  , Csv (..)
  , Encrypted (..)
  , FormData (..)
  , Gif (..)
  , Gzip (..)
  , Javascript (..)
  , Jpeg (..)
  , Json (..)
  , Mixed (..)
  , Mp4 (..)
  , Mpeg (..)
  , OctetStream (..)
  , Ogg (..)
  , Partial (..)
  , Plain (..)
  , Png (..)
  , Postscript (..)
  , Protobuf (..)
  , RdfXml (..)
  , RssXml (..)
  , Tar (..)
  , Tiff (..)
  , Thrift (..)
  , Vorbis (..)
  , Webm (..)
  , XhtmlXml (..)
  , Xml (..)
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

-- | Type-class for converting a 'ByteString' to a media-type.
class (Show a, Eq a) => MType a where
    toType :: a -> ByteString -> Maybe a

-- | Type-class for converting a 'ByteString' to a media-subtype.
class (Show a, Eq a) => MSubType a where
    toSubType :: a -> ByteString -> Maybe a

-- | The Media-type representation.
data MediaType t s = MediaType
  { _type    :: !t
  , _subtype :: !s
  , _quality :: !Double
  , _params  :: ![(ByteString, ByteString)]
  } deriving (Eq, Show)

-- | A 'Predicate' against the 'Request's "Accept" header.
data Accept t s = Accept t s deriving Eq

instance (MType t, MSubType s) => Predicate (Accept t s) Request where
    type FVal (Accept t s) = Error
    type TVal (Accept t s) = MediaType t s
    apply (Accept x y) r   = do
        mtypes <- E.lookup "accept" >>= maybe readMediaTypes return
        case mediaType x y mtypes of
               Just m  -> return (T (delta m) m)
               Nothing -> return (F (err 406 message))
      where
        readMediaTypes = do
            let mtypes = sortBy q . concat . map A.parseMediaTypes $ headers r "accept"
            E.insert "accept" mtypes
            return mtypes

        q a b = A.medQuality b `compare` A.medQuality a

        message = "Expected 'Accept: "
                    <> fromString (show x)
                    <> "/"
                    <> fromString (show y)
                    <> "'."

        delta m = [1.0 - _quality m]

instance (Show t, Show s) => Show (Accept t s) where
    show (Accept t s) = "Accept: " ++ show t ++ "/" ++ show s

mediaType :: (MType t, MSubType s) => t -> s -> [A.MediaType] -> Maybe (MediaType t s)
mediaType t s = safeHead . mapMaybe (\m -> do
    t' <- if A.medType    m == "*" then Just t else toType t    (A.medType m)
    s' <- if A.medSubtype m == "*" then Just s else toSubType s (A.medSubtype m)
    guard (t == t' && s == s')
    return $ MediaType t s (A.medQuality m) (A.medParams m))


-- Media-Types:

-- | Generic media-type.
data Type = Type ByteString deriving Eq

instance MType Type where
    toType o@(Type t) s = if t == s then Just o else Nothing

instance Show Type where
    show (Type t) = show t

defineType "Application" "application"
defineType "Audio" "audio"
defineType "Image" "image"
defineType "Message" "message"
defineType "Multipart" "multipart"
defineType "Text" "text"
defineType "Video" "video"

-- Media-Subtypes:

-- | Generic media-subtype.
data SubType = SubType ByteString deriving Eq

instance MSubType SubType where
    toSubType o@(SubType t) s = if t == s then Just o else Nothing

instance Show SubType where
    show (SubType t) = show t

defineSubType "AtomXml" "atom+xml"
defineSubType "Css" "css"
defineSubType "Csv" "csv"
defineSubType "Encrypted" "encrypted"
defineSubType "FormData" "form-data"
defineSubType "Gif" "gif"
defineSubType "Gzip" "gzip"
defineSubType "Javascript" "javascript"
defineSubType "Jpeg" "jpeg"
defineSubType "Json" "json"
defineSubType "Mixed" "mixed"
defineSubType "Mp4" "mp4"
defineSubType "Mpeg" "mpeg"
defineSubType "OctetStream" "octet-stream"
defineSubType "Ogg" "ogg"
defineSubType "Partial" "partial"
defineSubType "Plain" "plain"
defineSubType "Png" "png"
defineSubType "Postscript" "postscript"
defineSubType "Protobuf" "x-protobuf"
defineSubType "RdfXml" "rdf+xml"
defineSubType "RssXml" "rss+xml"
defineSubType "Tar" "tar"
defineSubType "Tiff" "tiff"
defineSubType "Thrift" "x-thrift"
defineSubType "Vorbis" "vorbis"
defineSubType "Webm" "webm"
defineSubType "XhtmlXml" "xhtml+xml"
defineSubType "Xml" "xml"

-- | media-type and sub-type \"*\".
data All = All deriving Eq

instance MType All where
    toType _ s = if s == "*" then Just All else Nothing

instance MSubType All where
    toSubType _ s = if s == "*" then Just All else Nothing

instance Show All where
    show _ = "*"
