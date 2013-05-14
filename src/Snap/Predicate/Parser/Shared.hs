{-# LANGUAGE OverloadedStrings #-}
module Snap.Predicate.Parser.Shared where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Word
import qualified Data.ByteString as S

spaces :: Parser ()
spaces = skipWhile (== w ' ')

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

oneof :: ByteString -> Word8 -> Bool
oneof s c = S.any (== c) s

chr :: Char -> Parser Word8
chr = word8 . w

w :: Char -> Word8
w = fromIntegral . ord
