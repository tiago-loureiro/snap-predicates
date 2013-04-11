{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates.Params
  ( Param (..)
  , ParamTrans (..)
  )
where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Typeable
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicates
import Snap.Predicates.Internal
import qualified Data.Predicate.Env as E

-- | A 'Predicate' looking for some parameter.
data Param = Param ByteString deriving Eq

instance Predicate Param Request where
    type FVal Param = Error
    type TVal Param = ByteString
    apply (Param x) r =
        case params r x of
            []    -> return (F (err 400 ("Expected parameter '" <> x <> "'.")))
            (v:_) -> return (T v)

instance Show Param where
    show (Param x) = "Param: " ++ show x

-- | A 'Predicate' looking for some parameter, and returning as
-- 'TVal' the result of applying the given function to all
-- parameter values.
data ParamTrans a = ParamTrans ByteString ([ByteString] -> a)

instance Typeable a => Predicate (ParamTrans a) Request where
    type FVal (ParamTrans a) = Error
    type TVal (ParamTrans a) = a
    apply (ParamTrans x f) r = E.lookup ("paramtrans:" <> x) >>= maybe work (return . T)
      where
        work = case params r x of
            [] -> return (F (err 400 ("Expected parameter '" <> x <> "'.")))
            vs -> do
                let result = f vs
                E.insert ("paramtrans:" <> x) result
                return (T result)

instance Show (ParamTrans a) where
    show (ParamTrans x _) = "ParamTrans: " ++ show x
