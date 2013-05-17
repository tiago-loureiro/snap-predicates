{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ByteString.Readable (Readable (..)) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text as T

-- | The type-class 'Readable' is used to convert 'ByteString' values to
-- values of other types. Most instances assume the 'ByteString' is
-- encoded via UTF-8.
--
-- Minimal complete instance definition is given by 'readByteString'.
class Readable a where
    readByteString :: Parser a

    -- | Instances can use this function to express their way of
    -- reading lists of values.
    readByteStringList :: Parser [a]
    readByteStringList = parseList

    -- | Turn the given 'ByteString' into a typed value.
    fromByteString :: ByteString -> Maybe a
    fromByteString = either (const Nothing) Just . parseOnly readByteString

parseList :: Readable a => Parser [a]
parseList = trim (char '[') *> go [] <* trim (char ']')
  where
    go !acc = peekChar >>= \c -> case c of
        Nothing  -> return (reverse acc)
        Just ']' -> return (reverse acc)
        _        -> do
            a <- readByteString
            spaces
            void $ optional (char ',')
            spaces
            go (a:acc)

instance Readable a => Readable [a] where
    readByteString = readByteStringList

instance Readable ByteString where
    readByteString = between '"' '"' return

instance Readable Text where
    readByteString = between '"' '"' text

instance Readable Char where
    readByteString = between '\'' '\'' $ \s -> case s of
        "" -> fail "no char"
        _  -> T.head <$> text s

    readByteStringList = between '"' '"' $ \s -> T.unpack <$> text s

instance Readable Double where
    readByteString = signed double

instance Readable Int where
    readByteString = signed decimal

-- Helpers:

text :: ByteString -> Parser Text
text = either (fail . show) return . decodeUtf8'

spaces :: Parser ()
spaces = skipWhile (== ' ')

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

between :: Char -> Char -> (ByteString -> Parser a) -> Parser a
between b e p = char b *> takeTill (== e) <* char e >>= p
