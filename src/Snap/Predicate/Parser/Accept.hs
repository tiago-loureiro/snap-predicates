{-# LANGUAGE OverloadedStrings, BangPatterns, DeriveDataTypeable #-}
module Snap.Predicate.Parser.Accept
  ( MediaType (..)
  , parseMediaTypes
  )
where

import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.Text (double)
import Data.ByteString (ByteString)
import Data.Text.Encoding
import Data.Typeable
import Snap.Predicate.Parser.Shared
import qualified Data.Attoparsec.Text as T

data MediaType = MediaType
  { medType    :: !ByteString
  , medSubtype :: !ByteString
  , medQuality :: !Double
  , medParams  :: ![(ByteString, ByteString)]
  } deriving (Eq, Show, Typeable)

parseMediaTypes :: ByteString -> [MediaType]
parseMediaTypes = either (const []) id . parseOnly mediaTypes

mediaTypes :: Parser [MediaType]
mediaTypes = mediaType `sepBy` chr ','

mediaType :: Parser MediaType
mediaType = toMediaType <$> trim typ <*> (chr '/' *> trim subtyp) <*> params
  where
    toMediaType t s p =
        case lookup "q" p >>= toDouble of
            Just q  -> MediaType t s q (filter ((/= "q") . fst) p)
            Nothing -> MediaType t s 1.0 p

params :: Parser [(ByteString, ByteString)]
params = (trim (chr ';') *> (element `sepBy` trim (chr ';'))) <|> return []
  where
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

toDouble :: ByteString -> Maybe Double
toDouble bs = do
    txt <- toMaybe (decodeUtf8' bs)
    dec <- toMaybe (T.parseOnly double txt)
    return dec
  where
    toMaybe (Right x) = Just x
    toMaybe (Left  _) = Nothing
