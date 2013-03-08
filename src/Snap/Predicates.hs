{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates
  ( Accept (..)
  , Param (..)
  )
where

import Data.ByteString (ByteString)
import Data.Monoid
import Predicates
import Snap.Core
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as M

-- | A 'Predicate' against the 'Request's "Accept" header.
data Accept = Accept ByteString

instance Predicate Accept Request where
    type Value Accept = ()
    apply (Accept x) r =
        if x `elem` headers' r "accept"
            then Yes ()
            else No 406 (Just "Expected 'Accept: accept/json'.")

instance Show Accept where
    show (Accept x) = "Accept: \"" ++ show x ++ "\""

-- | A 'Predicate' looking for some parameter value.
data Param  = Param ByteString

instance Predicate Param Request where
    type Value Param = ByteString
    apply (Param x) r =
        case params' r x of
            []    -> No 400 (Just ("Expected parameter " <> x <> "."))
            (v:_) -> Yes v

instance Show Param where
    show (Param x) = "Param: \"" ++ show x ++ "\""

-- Internal helpers:

headers' :: Request -> ByteString -> [ByteString]
headers' rq name = maybe [] id . getHeaders (CI.mk name) $ rq

params' :: Request -> ByteString -> [ByteString]
params' rq name = maybe [] id . M.lookup name . rqParams $ rq
