{-# LANGUAGE BangPatterns #-}
module Snap.Predicates.Internal
  ( headers
  , params
  , safeHead
  , replace
  )
where

import Snap.Core hiding (headers)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Char (ord)
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.ByteString as S

headers :: Request -> ByteString -> [ByteString]
headers rq name = maybe [] id . getHeaders (mk name) $ rq

params :: Request -> ByteString -> [ByteString]
params rq name = maybe [] id . M.lookup name . rqParams $ rq

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (h:_) = Just h

replace :: Char -> ByteString -> ByteString -> ByteString
replace !c !r !s
    | S.null s  = S.empty
    | otherwise = let (h, t) = S.break (== w) s
                  in h <> r <> (replace c r (S.drop 1 t))
  where
    w = fromIntegral (ord c)
