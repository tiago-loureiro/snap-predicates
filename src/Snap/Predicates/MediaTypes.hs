{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns          #-}
module Snap.Predicates.MediaTypes where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicates.Internal
import Snap.Predicates.Parsers.Accept

-- | A 'Predicate' against the 'Request's "Accept" header.
data Accept = Accept ByteString deriving Eq

instance Predicate Accept Request where
    type FVal Accept = (Word, Maybe ByteString)
    type TVal Accept = ()
    apply (Accept x) r =
        let mtypes = concat . map parseMediaTypes $ headers r "accept" in
        if x `elem` headers r "accept"
            then T ()
            else F $ Just (406, Just $ "Expected 'Accept: " <> x <> "'.")

instance Show Accept where
    show (Accept x) = "Accept: " ++ show x
