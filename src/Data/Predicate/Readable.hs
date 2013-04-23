module Data.Predicate.Readable (Readable (..)) where

import Data.ByteString (ByteString)
import Data.Attoparsec.Text
import Data.String
import Data.Text.Encoding (decodeUtf8')

-- | The type-class 'Readable' is used to convert 'ByteString' values to
-- values of other types. The instances for 'Double' and 'Int' assume
-- the 'ByteString' is encoded via UTF-8.
class Readable a where
    fromByteString :: ByteString -> Either ByteString a

instance Readable ByteString where
    fromByteString = Right

instance Readable Double where
    fromByteString = decode (signed double)

instance Readable Int where
    fromByteString = decode (signed decimal) -- TODO: Handle hexadecimal

instance Readable a => Readable (Maybe a) where
    fromByteString s =
        case fromByteString s of
            Left  _ -> Right Nothing
            Right x -> Right (Just x)

decode :: Parser a -> ByteString -> Either ByteString a
decode p s = mapLeft (decodeUtf8' s) >>= mapLeft . parseOnly p

mapLeft :: Show e => Either e a -> Either ByteString a
mapLeft (Left  s) = Left . fromString . show $ s
mapLeft (Right x) = Right x
