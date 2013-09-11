{-# LANGUAGE OverloadedStrings #-}

module Snap.Predicate.MediaType
  ( -- * Types
    MType (..)
  , MSubType (..)
  , MediaType (..)

  -- * Media-Types
  , All (..)
  , Application (..)
  , Audio (..)
  , Image (..)
  , Message (..)
  , Multipart (..)
  , Text (..)
  , Video (..)

  -- * Media-Sub-Types
  , AtomXml (..)
  , Css (..)
  , Csv (..)
  , Encrypted (..)
  , FormData (..)
  , FormUrlEncoded (..)
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
  , Pkcs12 (..)
  , Pkcs7Cert (..)
  , Pkcs7Sig (..)
  , Pkcs7Mime (..)
  , Pkcs7CertRqRs (..)
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
    toType :: ByteString -> Maybe a

-- | Type-class for converting a 'ByteString' to a media-subtype.
class (Show a, Eq a) => MSubType a where
    toSubType :: ByteString -> Maybe a

-- | The Media-type representation.
data MediaType t s = MediaType
  { _type    :: !t
  , _subtype :: !s
  , _quality :: !Double
  , _params  :: ![(ByteString, ByteString)]
  } deriving (Eq, Show)

-- Media-Types:

data Application = Application deriving Eq

instance MType Application where
    toType "application" = Just Application
    toType _             = Nothing

instance Show Application where
    show _ = "application"

data Audio = Audio deriving Eq

instance MType Audio where
    toType "audio" = Just Audio
    toType _       = Nothing

instance Show Audio where
    show _ = "audio"

data Image = Image deriving Eq

instance MType Image where
    toType "image" = Just Image
    toType _       = Nothing

instance Show Image where
    show _ = "image"

data Message = Message deriving Eq

instance MType Message where
    toType "message" = Just Message
    toType _         = Nothing

instance Show Message where
    show _ = "message"

data Multipart = Multipart deriving Eq

instance MType Multipart where
    toType "multipart" = Just Multipart
    toType _           = Nothing

instance Show Multipart where
    show _ = "multipart"

data Text = Text deriving Eq

instance MType Text where
    toType "text" = Just Text
    toType _      = Nothing

instance Show Text where
    show _ = "text"

data Video = Video deriving Eq

instance MType Video where
    toType "video" = Just Video
    toType _       = Nothing

instance Show Video where
    show _ = "video"

-- Media-Subtypes:

data AtomXml = AtomXml deriving Eq

instance MSubType AtomXml where
    toSubType "atom+xml" = Just AtomXml
    toSubType _          = Nothing

instance Show AtomXml where
    show _ = "atom+xml"

data Css = Css deriving Eq

instance MSubType Css where
    toSubType "css" = Just Css
    toSubType _     = Nothing

instance Show Css where
    show _ = "css"

data Csv = Csv deriving Eq

instance MSubType Csv where
    toSubType "csv" = Just Csv
    toSubType _     = Nothing

instance Show Csv where
    show _ = "csv"

data Encrypted = Encrypted deriving Eq

instance MSubType Encrypted where
    toSubType "encrypted" = Just Encrypted
    toSubType _           = Nothing

instance Show Encrypted where
    show _ = "encrypted"

data FormData = FormData deriving Eq

instance MSubType FormData where
    toSubType "form-data" = Just FormData
    toSubType _           = Nothing

instance Show FormData where
    show _ = "form-data"

data FormUrlEncoded = FormUrlEncoded deriving Eq

instance MSubType FormUrlEncoded where
    toSubType "x-www-form-urlencoded" = Just FormUrlEncoded
    toSubType _                       = Nothing

instance Show FormUrlEncoded where
    show _ = "x-www-form-urlencoded"

data Gif = Gif deriving Eq

instance MSubType Gif where
    toSubType "gif" = Just Gif
    toSubType _     = Nothing

instance Show Gif where
    show _ = "gif"

data Gzip = Gzip deriving Eq

instance MSubType Gzip where
    toSubType "gzip" = Just Gzip
    toSubType _      = Nothing

instance Show Gzip where
    show _ = "gzip"

data Javascript = Javascript deriving Eq

instance MSubType Javascript where
    toSubType "javascript" = Just Javascript
    toSubType _            = Nothing

instance Show Javascript where
    show _ = "javascript"

data Jpeg = Jpeg deriving Eq

instance MSubType Jpeg where
    toSubType "jpeg" = Just Jpeg
    toSubType _      = Nothing

instance Show Jpeg where
    show _ = "jpeg"

data Json = Json deriving Eq

instance MSubType Json where
    toSubType "json" = Just Json
    toSubType _      = Nothing

instance Show Json where
    show _ = "json"

data Mixed = Mixed deriving Eq

instance MSubType Mixed where
    toSubType "mixed" = Just Mixed
    toSubType _       = Nothing

instance Show Mixed where
    show _ = "mixed"

data Mp4 = Mp4 deriving Eq

instance MSubType Mp4 where
    toSubType "mp4" = Just Mp4
    toSubType _     = Nothing

instance Show Mp4 where
    show _ = "mp4"

data Mpeg = Mpeg deriving Eq

instance MSubType Mpeg where
    toSubType "mpeg" = Just Mpeg
    toSubType _      = Nothing

instance Show Mpeg where
    show _ = "mpeg"

data OctetStream = OctetStream deriving Eq

instance MSubType OctetStream where
    toSubType "octet-stream" = Just OctetStream
    toSubType _              = Nothing

instance Show OctetStream where
    show _ = "octet-stream"

data Ogg = Ogg deriving Eq

instance MSubType Ogg where
    toSubType "ogg" = Just Ogg
    toSubType _     = Nothing

instance Show Ogg where
    show _ = "ogg"

data Partial = Partial deriving Eq

instance MSubType Partial where
    toSubType "partial" = Just Partial
    toSubType _         = Nothing

instance Show Partial where
    show _ = "partial"

data Pkcs12 = Pkcs12 deriving Eq

instance MSubType Pkcs12 where
    toSubType "x-pkcs12" = Just Pkcs12
    toSubType _          = Nothing

instance Show Pkcs12 where
    show _ = "x-pkcs12"

data Pkcs7Cert = Pkcs7Cert deriving Eq

instance MSubType Pkcs7Cert where
    toSubType "x-pkcs7-certificates" = Just Pkcs7Cert
    toSubType _                      = Nothing

instance Show Pkcs7Cert where
    show _ = "x-pkcs7-certificates"

data Pkcs7Sig = Pkcs7Sig deriving Eq

instance MSubType Pkcs7Sig where
    toSubType "x-pkcs7-signature" = Just Pkcs7Sig
    toSubType _                   = Nothing

instance Show Pkcs7Sig where
    show _ = "x-pkcs7-signature"

data Pkcs7Mime = Pkcs7Mime deriving Eq

instance MSubType Pkcs7Mime where
    toSubType "x-pkcs7-mime" = Just Pkcs7Mime
    toSubType _              = Nothing

instance Show Pkcs7Mime where
    show _ = "x-pkcs7-mime"

data Pkcs7CertRqRs = Pkcs7CertRqRs deriving Eq

instance MSubType Pkcs7CertRqRs where
    toSubType "x-pkcs7-certreqresp" = Just Pkcs7CertRqRs
    toSubType _                     = Nothing

instance Show Pkcs7CertRqRs where
    show _ = "x-pkcs7-certreqresp"

data Plain = Plain deriving Eq

instance MSubType Plain where
    toSubType "plain" = Just Plain
    toSubType _       = Nothing

instance Show Plain where
    show _ = "plain"

data Png = Png deriving Eq

instance MSubType Png where
    toSubType "png" = Just Png
    toSubType _     = Nothing

instance Show Png where
    show _ = "png"

data Postscript = Postscript deriving Eq

instance MSubType Postscript where
    toSubType "postscript" = Just Postscript
    toSubType _            = Nothing

instance Show Postscript where
    show _ = "postscript"

data Protobuf = Protobuf deriving Eq

instance MSubType Protobuf where
    toSubType "x-protobuf" = Just Protobuf
    toSubType _            = Nothing

instance Show Protobuf where
    show _ = "x-protobuf"

data RdfXml = RdfXml deriving Eq

instance MSubType RdfXml where
    toSubType "rdf+xml" = Just RdfXml
    toSubType _         = Nothing

instance Show RdfXml where
    show _ = "rdf+xml"

data RssXml = RssXml deriving Eq

instance MSubType RssXml where
    toSubType "rss+xml" = Just RssXml
    toSubType _         = Nothing

instance Show RssXml where
    show _ = "rss+xml"

data Tar = Tar deriving Eq

instance MSubType Tar where
    toSubType "tar" = Just Tar
    toSubType _     = Nothing

instance Show Tar where
    show _ = "tar"

data Tiff = Tiff deriving Eq

instance MSubType Tiff where
    toSubType "tiff" = Just Tiff
    toSubType _      = Nothing

instance Show Tiff where
    show _ = "tiff"

data Thrift = Thrift deriving Eq

instance MSubType Thrift where
    toSubType "x-thrift" = Just Thrift
    toSubType _          = Nothing

instance Show Thrift where
    show _ = "x-thrift"

data Vorbis = Vorbis deriving Eq

instance MSubType Vorbis where
    toSubType "vorbis" = Just Vorbis
    toSubType _        = Nothing

instance Show Vorbis where
    show _ = "vorbis"

data Webm = Webm deriving Eq

instance MSubType Webm where
    toSubType "webm" = Just Webm
    toSubType _      = Nothing

instance Show Webm where
    show _ = "webm"

data XhtmlXml = XhtmlXml deriving Eq

instance MSubType XhtmlXml where
    toSubType "xhtml+xml" = Just XhtmlXml
    toSubType _           = Nothing

instance Show XhtmlXml where
    show _ = "xhtml+xml"

data Xml = Xml deriving Eq

instance MSubType Xml where
    toSubType "xml" = Just Xml
    toSubType _     = Nothing

instance Show Xml where
    show _ = "xml"

-- | media-type and sub-type \"*\".
data All = All deriving Eq

instance MType All where
    toType "*" = Just All
    toType _   = Nothing

instance MSubType All where
    toSubType "*" = Just All
    toSubType _   = Nothing

instance Show All where
    show _ = "*"

