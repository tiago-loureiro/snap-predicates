{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates
  ( Accept (..)
  , Param (..)
  , ParamTrans (..)
  , AcceptJson (..)
  , AcceptThrift (..)
  , AcceptOctets (..)
  , AcceptXml (..)
  , AcceptText (..)
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
data Accept = Accept ByteString deriving Eq

instance Predicate Accept Request where
    type FVal Accept = (Word, Maybe ByteString)
    type TVal Accept = ()
    apply (Accept x) r =
        if x `elem` headers' r "accept"
            then T ()
            else F $ Just (406, Just $ "Expected 'Accept: " <> x <> "'.")

instance Show Accept where
    show (Accept x) = "Accept: " ++ show x

-- | A 'Predicate' looking for some parameter value.
data Param = Param ByteString deriving Eq

instance Predicate Param Request where
    type FVal Param = (Word, Maybe ByteString)
    type TVal Param = ByteString
    apply (Param x) r =
        case params' r x of
            []    -> F $ Just (400, Just $ "Expected parameter '" <> x <> "'.")
            (v:_) -> T v

instance Show Param where
    show (Param x) = "Param: " ++ show x

-- | A 'Predicate' looking for some parameter value, and
-- returning as 'TVal' the result of applying the given function
-- to all parameter values.
data ParamTrans a = ParamTrans ByteString ([ByteString] -> a)

instance Predicate (ParamTrans a) Request where
    type FVal (ParamTrans a) = (Word, Maybe ByteString)
    type TVal (ParamTrans a) = a
    apply (ParamTrans x f) r =
        case params' r x of
            [] -> F $ Just (400, Just $ "Expected parameter '" <> x <> "'.")
            vs -> T $ f vs

instance Show (ParamTrans a) where
    show (ParamTrans x _) = "ParamTrans: " ++ show x

-- | A 'Predicate' which is true only for Accept: \"application/json\".
data AcceptJson = AcceptJson deriving Eq

instance Predicate AcceptJson Request where
    type FVal AcceptJson = (Word, Maybe ByteString)
    type TVal AcceptJson = AcceptJson
    apply AcceptJson r =
        apply (Accept "application/json") r >> return AcceptJson

instance Show AcceptJson where
    show AcceptJson = "Accept: \"application/json\""

-- | A 'Predicate' which is true only for Accept: \"application/x-thrift\".
data AcceptThrift = AcceptThrift deriving Eq

instance Predicate AcceptThrift Request where
    type FVal AcceptThrift = (Word, Maybe ByteString)
    type TVal AcceptThrift = AcceptThrift
    apply AcceptThrift r =
        apply (Accept "application/x-thrift") r >> return AcceptThrift

instance Show AcceptThrift where
    show AcceptThrift = "Accept: \"application/x-thrift\""

-- | A 'Predicate' which is true only for Accept: \"application/octet-stream\".
data AcceptOctets = AcceptOctets deriving Eq

instance Predicate AcceptOctets Request where
    type FVal AcceptOctets = (Word, Maybe ByteString)
    type TVal AcceptOctets = AcceptOctets
    apply AcceptOctets r =
        apply (Accept "application/octet-stream") r >> return AcceptOctets

instance Show AcceptOctets where
    show AcceptOctets = "Accept: \"application/octet-stream\""

-- | A 'Predicate' which is true only for Accept: \"application/xml\".
data AcceptXml = AcceptXml deriving Eq

instance Predicate AcceptXml Request where
    type FVal AcceptXml = (Word, Maybe ByteString)
    type TVal AcceptXml = AcceptXml
    apply AcceptXml r =
        apply (Accept "application/xml") r >> return AcceptXml

instance Show AcceptXml where
    show AcceptXml = "Accept: \"application/xml\""

-- | A 'Predicate' which is true only for Accept: \"text/plain\".
data AcceptText = AcceptText deriving Eq

instance Predicate AcceptText Request where
    type FVal AcceptText = (Word, Maybe ByteString)
    type TVal AcceptText = AcceptText
    apply AcceptText r =
        apply (Accept "text/plain") r >> return AcceptText

instance Show AcceptText where
    show AcceptText = "Accept: \"text/plain\""

-- Internal helpers:

headers' :: Request -> ByteString -> [ByteString]
headers' rq name = maybe [] id . getHeaders (CI.mk name) $ rq

params' :: Request -> ByteString -> [ByteString]
params' rq name = maybe [] id . M.lookup name . rqParams $ rq
