{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Snap.Predicates
  ( Accept (..)
  , Param (..)
  )
where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word
import Data.Predicate
import Snap.Core
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as M

-- | A 'Predicate' against the 'Request's "Accept" header.
data Accept = Accept ByteString

instance Predicate Accept Request (Word, Maybe ByteString) () where
    apply (Accept x) r =
        if x `elem` headers' r "accept"
            then T ()
            else F $ Just (406, Just $ "Expected 'Accept: " <> x <> ".")

instance Show Accept where
    show (Accept x) = "Accept: " ++ show x

-- | A 'Predicate' looking for some parameter value.
data Param  = Param ByteString

instance Predicate Param Request (Word, Maybe ByteString) ByteString where
    apply (Param x) r =
        case params' r x of
            []    -> F $ Just (400, Just $ "Expected parameter '" <> x <> "'.")
            (v:_) -> T v

instance Show Param where
    show (Param x) = "Param: " ++ show x

-- Internal helpers:

headers' :: Request -> ByteString -> [ByteString]
headers' rq name = maybe [] id . getHeaders (CI.mk name) $ rq

params' :: Request -> ByteString -> [ByteString]
params' rq name = maybe [] id . M.lookup name . rqParams $ rq
