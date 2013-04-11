{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Snap.Predicates.Parsers.Shared where

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
oneof = flip elem . S.unpack

chr :: Char -> Parser Word8
chr = word8 . w

w :: Char -> Word8
w = fromIntegral . ord