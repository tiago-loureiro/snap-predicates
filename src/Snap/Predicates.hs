{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates
  ( Param (..)
  , ParamTrans (..)
  )
where

import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Typeable
import Data.Word
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicates.Internal
import qualified Data.Predicate.Env as E

-- | A 'Predicate' looking for some parameter.
data Param = Param ByteString deriving Eq

instance Predicate Param Request where
    type FVal Param = (Word, Maybe ByteString)
    type TVal Param = ByteString
    apply (Param x) r = do
        value <- E.lookup ("param:" <> x)
        case value of
            Just v  -> return v
            Nothing -> case params r x of
                []    -> StateT $ const (F $ Just (400, Just $ "Expected parameter '" <> x <> "'."))
                (v:_) -> E.insert ("param:" <> x) v >> return v

instance Show Param where
    show (Param x) = "Param: " ++ show x

-- | A 'Predicate' looking for some parameter, and returning as
-- 'TVal' the result of applying the given function to all
-- parameter values.
data ParamTrans a = ParamTrans ByteString ([ByteString] -> a)

instance Typeable a => Predicate (ParamTrans a) Request where
    type FVal (ParamTrans a) = (Word, Maybe ByteString)
    type TVal (ParamTrans a) = a
    apply (ParamTrans x f) r = E.lookup ("paramtrans:" <> x) >>= maybe work return
      where
        work = case params r x of
            [] -> StateT $ const (F $ Just (400, Just $ "Expected parameter '" <> x <> "'."))
            vs -> do
                let result = f vs
                E.insert ("paramtrans:" <> x) result
                return result

instance Show (ParamTrans a) where
    show (ParamTrans x _) = "ParamTrans: " ++ show x
