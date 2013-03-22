{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Snap.Predicates.Parsers.Accept
  ( MediaType (..)
  , Type (..)
  , SubType (..)
  , parseMediaTypes
  )
where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Word
import qualified Data.ByteString as S

data Type =
   AllTypes
 | Application
 | Audio
 | Image
 | Message
 | Model
 | Text
 | Video
 | Type !ByteString
 deriving (Eq, Show)

data SubType =
   Thrift
 | Json
 | Xml
 | Html
 | Octet
 | Plain
 | AllSubTypes
 | SubType !ByteString
 deriving (Eq, Show)

toType :: ByteString -> Type
toType "*"           = AllTypes
toType "application" = Application
toType "audio"       = Audio
toType "image"       = Image
toType "message"     = Message
toType "model"       = Model
toType "text"        = Text
toType "video"       = Video
toType other         = Type other

toSubType :: ByteString -> SubType
toSubType "*"        = AllSubTypes
toSubType "x-thrift" = Thrift
toSubType "json"     = Json
toSubType "xml"      = Xml
toSubType "html"     = Html
toSubType "octet"    = Octet
toSubType "plain"    = Plain
toSubType other      = SubType other

data MediaType = MediaType
  { medType    :: !Type
  , medSubtype :: !SubType
  , medQuality :: !ByteString
  , medParams  :: ![(ByteString, ByteString)]
  } deriving (Eq, Show)

parseMediaTypes :: ByteString -> [MediaType]
parseMediaTypes = either (const []) id . parseOnly mediaTypes

mediaTypes :: Parser [MediaType]
mediaTypes = mediaType `sepBy` chr ','

mediaType :: Parser MediaType
mediaType = toMediaType <$> trim typ <*> (chr '/' *> trim subtyp) <*> params
  where
    toMediaType t s p =
        let ty = toType t
            sb = toSubType s
        in case lookup "q" p of
            Just q  -> MediaType ty sb q (filter ((/= "q") . fst) p)
            Nothing -> MediaType ty sb "1.0" p

params :: Parser [(ByteString, ByteString)]
params = (trim (chr ';') *> (element `sepBy` trim (chr ';'))) <|> return []

element :: Parser (ByteString, ByteString)
element = (,) <$> trim key <*> (chr '=' *> trim val)

typ, subtyp, key, val :: Parser ByteString
typ    = takeTill (oneof "/ ")
subtyp = takeTill (oneof ",; ")

key = do
    c <- peekWord8
    if c == Just (w ',')
        then fail "comma"
        else takeTill (oneof "= ")

val = takeTill (oneof ",; ")

spaces :: Parser ()
spaces = skipWhile (== w ' ')

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

oneof :: ByteString -> Word8 -> Bool
oneof = flip elem . S.unpack

chr :: Char -> Parser Word8
chr = word8 . w

w :: Char -> Word8
w = fromIntegral . ord
