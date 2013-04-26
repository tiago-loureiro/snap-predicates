{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
module Snap.Predicates.MediaTypes
  ( -- * Types
    MType (..)
  , MSubType (..)
  , MediaType (..)

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

import Data.ByteString (ByteString)

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

-- Media-Types:

-- | Generic media-type.
data Type = Type ByteString deriving Eq

instance MType Type where
    toType o@(Type t) s = if t == s then Just o else Nothing

instance Show Type where
    show (Type t) = show t

data Application = Application deriving Eq

instance MType Application where
    toType _ "application" = Just Application
    toType _ _             = Nothing

instance Show Application where
    show _ = "application"

data Audio = Audio deriving Eq

instance MType Audio where
    toType _ "audio" = Just Audio
    toType _ _       = Nothing

instance Show Audio where
    show _ = "audio"

data Image = Image deriving Eq

instance MType Image where
    toType _ "image" = Just Image
    toType _ _       = Nothing

instance Show Image where
    show _ = "image"

data Message = Message deriving Eq

instance MType Message where
    toType _ "message" = Just Message
    toType _ _         = Nothing

instance Show Message where
    show _ = "message"

data Multipart = Multipart deriving Eq

instance MType Multipart where
    toType _ "multipart" = Just Multipart
    toType _ _           = Nothing

instance Show Multipart where
    show _ = "multipart"

data Text = Text deriving Eq

instance MType Text where
    toType _ "text" = Just Text
    toType _ _      = Nothing

instance Show Text where
    show _ = "text"

data Video = Video deriving Eq

instance MType Video where
    toType _ "video" = Just Video
    toType _ _       = Nothing

instance Show Video where
    show _ = "video"

-- Media-Subtypes:

-- | Generic media-subtype.
data SubType = SubType ByteString deriving Eq

instance MSubType SubType where
    toSubType o@(SubType t) s = if t == s then Just o else Nothing

instance Show SubType where
    show (SubType t) = show t

data AtomXml = AtomXml deriving Eq

instance MSubType AtomXml where
    toSubType _ "atom+xml" = Just AtomXml
    toSubType _ _          = Nothing

instance Show AtomXml where
    show _ = "atom+xml"

data Css = Css deriving Eq

instance MSubType Css where
    toSubType _ "css" = Just Css
    toSubType _ _     = Nothing

instance Show Css where
    show _ = "css"

data Csv = Csv deriving Eq

instance MSubType Csv where
    toSubType _ "csv" = Just Csv
    toSubType _ _     = Nothing

instance Show Csv where
    show _ = "csv"

data Encrypted = Encrypted deriving Eq

instance MSubType Encrypted where
    toSubType _ "encrypted" = Just Encrypted
    toSubType _ _           = Nothing

instance Show Encrypted where
    show _ = "encrypted"

data FormData = FormData deriving Eq

instance MSubType FormData where
    toSubType _ "form-data" = Just FormData
    toSubType _ _           = Nothing

instance Show FormData where
    show _ = "form-data"

data Gif = Gif deriving Eq

instance MSubType Gif where
    toSubType _ "gif" = Just Gif
    toSubType _ _     = Nothing

instance Show Gif where
    show _ = "gif"

data Gzip = Gzip deriving Eq

instance MSubType Gzip where
    toSubType _ "gzip" = Just Gzip
    toSubType _ _      = Nothing

instance Show Gzip where
    show _ = "gzip"

data Javascript = Javascript deriving Eq

instance MSubType Javascript where
    toSubType _ "javascript" = Just Javascript
    toSubType _ _            = Nothing

instance Show Javascript where
    show _ = "javascript"

data Jpeg = Jpeg deriving Eq

instance MSubType Jpeg where
    toSubType _ "jpeg" = Just Jpeg
    toSubType _ _      = Nothing

instance Show Jpeg where
    show _ = "jpeg"

data Json = Json deriving Eq

instance MSubType Json where
    toSubType _ "json" = Just Json
    toSubType _ _      = Nothing

instance Show Json where
    show _ = "json"

data Mixed = Mixed deriving Eq

instance MSubType Mixed where
    toSubType _ "mixed" = Just Mixed
    toSubType _ _       = Nothing

instance Show Mixed where
    show _ = "mixed"

data Mp4 = Mp4 deriving Eq

instance MSubType Mp4 where
    toSubType _ "mp4" = Just Mp4
    toSubType _ _     = Nothing

instance Show Mp4 where
    show _ = "mp4"

data Mpeg = Mpeg deriving Eq

instance MSubType Mpeg where
    toSubType _ "mpeg" = Just Mpeg
    toSubType _ _      = Nothing

instance Show Mpeg where
    show _ = "mpeg"

data OctetStream = OctetStream deriving Eq

instance MSubType OctetStream where
    toSubType _ "octet-stream" = Just OctetStream
    toSubType _ _              = Nothing

instance Show OctetStream where
    show _ = "octet-stream"

data Ogg = Ogg deriving Eq

instance MSubType Ogg where
    toSubType _ "ogg" = Just Ogg
    toSubType _ _     = Nothing

instance Show Ogg where
    show _ = "ogg"

data Partial = Partial deriving Eq

instance MSubType Partial where
    toSubType _ "partial" = Just Partial
    toSubType _ _         = Nothing

instance Show Partial where
    show _ = "partial"

data Plain = Plain deriving Eq

instance MSubType Plain where
    toSubType _ "plain" = Just Plain
    toSubType _ _       = Nothing

instance Show Plain where
    show _ = "plain"

data Png = Png deriving Eq

instance MSubType Png where
    toSubType _ "png" = Just Png
    toSubType _ _     = Nothing

instance Show Png where
    show _ = "png"

data Postscript = Postscript deriving Eq

instance MSubType Postscript where
    toSubType _ "postscript" = Just Postscript
    toSubType _ _            = Nothing

instance Show Postscript where
    show _ = "postscript"

data Protobuf = Protobuf deriving Eq

instance MSubType Protobuf where
    toSubType _ "x-protobuf" = Just Protobuf
    toSubType _ _            = Nothing

instance Show Protobuf where
    show _ = "x-protobuf"

data RdfXml = RdfXml deriving Eq

instance MSubType RdfXml where
    toSubType _ "rdf+xml" = Just RdfXml
    toSubType _ _         = Nothing

instance Show RdfXml where
    show _ = "rdf+xml"

data RssXml = RssXml deriving Eq

instance MSubType RssXml where
    toSubType _ "rss+xml" = Just RssXml
    toSubType _ _         = Nothing

instance Show RssXml where
    show _ = "rss+xml"

data Tar = Tar deriving Eq

instance MSubType Tar where
    toSubType _ "tar" = Just Tar
    toSubType _ _     = Nothing

instance Show Tar where
    show _ = "tar"

data Tiff = Tiff deriving Eq

instance MSubType Tiff where
    toSubType _ "tiff" = Just Tiff
    toSubType _ _      = Nothing

instance Show Tiff where
    show _ = "tiff"

data Thrift = Thrift deriving Eq

instance MSubType Thrift where
    toSubType _ "x-thrift" = Just Thrift
    toSubType _ _          = Nothing

instance Show Thrift where
    show _ = "x-thrift"

data Vorbis = Vorbis deriving Eq

instance MSubType Vorbis where
    toSubType _ "vorbis" = Just Vorbis
    toSubType _ _        = Nothing

instance Show Vorbis where
    show _ = "vorbis"

data Webm = Webm deriving Eq

instance MSubType Webm where
    toSubType _ "webm" = Just Webm
    toSubType _ _      = Nothing

instance Show Webm where
    show _ = "webm"

data XhtmlXml = XhtmlXml deriving Eq

instance MSubType XhtmlXml where
    toSubType _ "xhtml+xml" = Just XhtmlXml
    toSubType _ _           = Nothing

instance Show XhtmlXml where
    show _ = "xhtml+xml"

data Xml = Xml deriving Eq

instance MSubType Xml where
    toSubType _ "xml" = Just Xml
    toSubType _ _     = Nothing

instance Show Xml where
    show _ = "xml"

-- | media-type and sub-type \"*\".
data All = All deriving Eq

instance MType All where
    toType _ "*" = Just All
    toType _ _   = Nothing

instance MSubType All where
    toSubType _ "*" = Just All
    toSubType _ _   = Nothing

instance Show All where
    show _ = "*"
