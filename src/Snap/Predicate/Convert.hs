{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Snap.Predicate.Convert (Convert (..)) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

-- | Parsing of 'ByteString's, producing values.
--
-- Minimal complete definition: 'convert'.
-- Most instances assume the 'ByteString' is encoded via UTF-8.
class Convert a where
    -- | Parse the given 'ByteString' into a typed value and also return
    -- the unconsumed bytes.
    convert :: ByteString -> Maybe (a, ByteString)

    -- | Parse the given 'ByteString' into a list of typed values and
    -- return the unconsumed bytes. Instances can use this function
    -- to express their way of reading lists of values. The default
    -- implementation parses comma-separated values (also accepting
    -- interspersed spaces).
    convertList :: ByteString -> Maybe ([a], ByteString)
    convertList s = parseList ([], s)

    -- | Turn the given 'ByteString' into a typed value.
    -- This function also checks, that all input bytes have been
    -- consumed or else it will fail.
    fromByteString :: ByteString -> Maybe a
    fromByteString s = case convert s of
        Just (v, s') | C.null s' -> Just v
        _                        -> Nothing

-- | Please note that lists are a flat sequence of comma-separated values
-- without brackets.
instance Convert a => Convert [a] where
    convert = convertList

instance Convert ByteString where
    convert = Just . (, "")

instance Convert Text where
    convert s = (, "") <$> fromEither (decodeUtf8' s)

instance Convert Char where
    convert s = do
        t <- fromEither (decodeUtf8' s)
        guard (T.length t == 1)
        return (T.head t, "")

    convertList s = (, "") . T.unpack <$> fromEither (decodeUtf8' s)

instance Convert Double where
    convert = runParser (signed double)

instance Convert Int where
    convert = runParser (signed decimal)

parseList :: Convert a => ([a], ByteString) -> Maybe ([a], ByteString)
parseList !(!acc, !s)
    | C.null s  = return (reverse acc, s)
    | otherwise = do
        (a, s') <- convert s
        parseList (a:acc, C.dropWhile noise s')
  where
    noise w = w `elem` [' ', ',']

fromEither :: Either e a -> Maybe a
fromEither = either (const Nothing) Just

runParser :: Parser a -> ByteString -> Maybe (a, ByteString)
runParser p = result . parse p
  where
    result (Done      s a) = Just (a, s)
    result (Partial     k) = result (k "")
    result (Fail    _ _ _) = Nothing
