{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates.MediaTypes where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicates.Internal

-- | A 'Predicate' against the 'Request's "Accept" header.
data Accept = Accept ByteString deriving Eq

instance Predicate Accept Request where
    type FVal Accept = (Word, Maybe ByteString)
    type TVal Accept = ()
    apply (Accept x) r =
        if x `elem` headers r "accept"
            then T ()
            else F $ Just (406, Just $ "Expected 'Accept: " <> x <> "'.")

instance Show Accept where
    show (Accept x) = "Accept: " ++ show x

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
