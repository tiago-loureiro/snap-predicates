{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates
  ( Param (..)
  , ParamTrans (..)
  )
where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicates.Internal

-- | A 'Predicate' looking for some parameter.
data Param = Param ByteString deriving Eq

instance Predicate Param Request where
    type FVal Param = (Word, Maybe ByteString)
    type TVal Param = ByteString
    apply (Param x) r =
        case params r x of
            []    -> F $ Just (400, Just $ "Expected parameter '" <> x <> "'.")
            (v:_) -> T v

instance Show Param where
    show (Param x) = "Param: " ++ show x

-- | A 'Predicate' looking for some parameter, and returning as
-- 'TVal' the result of applying the given function to all
-- parameter values.
data ParamTrans a = ParamTrans ByteString ([ByteString] -> a)

instance Predicate (ParamTrans a) Request where
    type FVal (ParamTrans a) = (Word, Maybe ByteString)
    type TVal (ParamTrans a) = a
    apply (ParamTrans x f) r =
        case params r x of
            [] -> F $ Just (400, Just $ "Expected parameter '" <> x <> "'.")
            vs -> T $ f vs

instance Show (ParamTrans a) where
    show (ParamTrans x _) = "ParamTrans: " ++ show x
