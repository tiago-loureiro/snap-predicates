{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates
  ( Accept (..)
  , AcceptJson (..)
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

instance Predicate Accept Request where
    type FVal Accept = (Word, Maybe ByteString)
    type TVal Accept = ()
    apply (Accept x) r =
        if x `elem` headers' r "accept"
            then T ()
            else F $ Just (406, Just $ "Expected 'Accept: " <> x <> "'.")

instance Show Accept where
    show (Accept x) = "Accept: " ++ show x

-- | A 'Predicate' which is true only for Accept: "application/json".
data AcceptJson = AcceptJson

instance Predicate AcceptJson Request where
    type FVal AcceptJson = (Word, Maybe ByteString)
    type TVal AcceptJson = AcceptJson
    apply AcceptJson r =
        apply (Accept "application/json") r >> return AcceptJson

instance Show AcceptJson where
    show AcceptJson = "Accept: \"application/json\""

-- | A 'Predicate' looking for some parameter value.
data Param  = Param ByteString

instance Predicate Param Request where
    type FVal Param = (Word, Maybe ByteString)
    type TVal Param = ByteString
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
