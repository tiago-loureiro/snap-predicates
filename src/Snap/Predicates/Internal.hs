{-# LANGUAGE OverloadedStrings #-}
module Snap.Predicates.Internal
  ( headers
  , params
  , safeHead
  , readValues
  )
where

import Snap.Core hiding (headers)
import Data.ByteString (ByteString)
import Data.ByteString.Readable
import Data.CaseInsensitive (mk)
import Data.Either

import qualified Data.ByteString as S
import qualified Data.Map.Strict as M

headers :: Request -> ByteString -> [ByteString]
headers rq name = maybe [] id . getHeaders (mk name) $ rq

params :: Request -> ByteString -> [ByteString]
params rq name = maybe [] id . M.lookup name . rqParams $ rq

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (h:_) = Just h

readValues :: Readable a => [ByteString] -> Either ByteString a
readValues vs =
    let (es, xs) = partitionEithers $ map fromByteString vs
    in if null xs
           then Left (S.intercalate "\n" es)
           else Right (head xs)
