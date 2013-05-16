{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Data.ByteString.Readable (Readable (..)) where

import Control.Applicative
import Data.Attoparsec hiding (parse)
import Data.ByteString (ByteString, snoc)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Read
import Snap.Predicate.Parser.Shared

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

-- | The type-class 'Readable' is used to convert 'ByteString' values to
-- values of other types. Most instances assume the 'ByteString' is
-- encoded via UTF-8.
--
-- Minimal complete instance definition is given by 'readByteString'.
class Readable a where
    -- | Parse the given 'ByteString' into a typed value and also return
    -- the unconsumed bytes. In case of error, provide an error message.
    readByteString :: ByteString -> Either ByteString (a, ByteString)

    -- | Parse the given 'ByteString' into a list of typed values and
    -- return the unconsumed bytes. In case of error, provide an error
    -- message. Instances can use this function to express their way of
    -- reading lists of values. The default implementation parses
    -- comma-separated values (also accepting interspersed spaces).
    readByteStringList :: ByteString -> Either ByteString ([a], ByteString)
    readByteStringList = parseList

    -- | Either turn the given 'ByteString' into a typed value or an error
    -- message. This function also checks, that all input bytes have been
    -- consumed or else it will fail.
    fromByteString :: ByteString -> Either ByteString a
    fromByteString s = do
        (v, s') <- readByteString s
        if S.null s'
            then Right v
            else Left ("unconsumed bytes: '" <> s' <> "'")

parseList :: Readable a => ByteString -> Either ByteString ([a], ByteString)
parseList = either (const (Left "no parse")) (return . (, "")) . parseOnly parser
  where
    parser  = trim (chr '[') *> go [] <* trim (chr ']')
    go !acc = peekWord8 >>= \c -> case c of
        Nothing   -> return (reverse acc)
        Just 0x5D -> return (reverse acc)
        Just 0x5B -> takeTill (== 0x5D) >>= \s -> anyWord8 >>= readVal acc . snoc s
        _         -> takeTill (oneof " ,]") >>= readVal acc

    readVal !acc !s = case fromByteString s of
        Left  e -> fail (C.unpack e)
        Right x -> optional comma >> spaces >> go (x:acc)

    comma = spaces >> chr ','

instance Readable a => Readable [a] where
    readByteString = readByteStringList

instance Readable ByteString where
    readByteString = Right . (, "")

instance Readable Text where
    readByteString s = (, "") <$> mapLeft (decodeUtf8' s)

instance Readable Char where
    readByteString s = do
        t <- mapLeft (decodeUtf8' s)
        return (T.head t, encodeUtf8 (T.tail t))

    readByteStringList s = (, "") . T.unpack <$> mapLeft (decodeUtf8' s)

instance Readable Double where
    readByteString = parse (signed double)

instance Readable Int where
    readByteString = parse (signed decimal)

mapLeft :: Show e => Either e a -> Either ByteString a
mapLeft (Left  s) = Left (encodeUtf8 . T.pack . show $ s)
mapLeft (Right x) = Right x

parse :: (Text -> Either String (a, Text)) -> ByteString -> Either ByteString (a, ByteString)
parse f s = do
    t <- mapLeft (decodeUtf8' s)
    case f t of
        Left       e  -> Left (encodeUtf8 (T.pack e))
        Right (v, t') -> return (v, encodeUtf8 t')
