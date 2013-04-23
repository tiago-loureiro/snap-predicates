{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates.Params
  ( Param (..)
  , ParamTrans (..)
  , ParamGuard (..)
  )
where

import Data.ByteString (ByteString)
import Data.List (find)
import Data.Monoid
import Data.Typeable
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicates
import Snap.Predicates.Internal
import qualified Data.Predicate.Delta as D
import qualified Data.Predicate.Env as E

-- | A 'Predicate' looking for some parameter.
data Param = Param ByteString deriving Eq

instance Predicate Param Request where
    type FVal Param = Error
    type TVal Param = ByteString
    apply (Param x) r =
        case params r x of
            []    -> return (F (err 400 ("Expected parameter '" <> x <> "'.")))
            (v:_) -> return (T D.empty v)

instance Show Param where
    show (Param x) = "Param: " ++ show x

-- | A 'Predicate' looking for some parameter, and returning as
-- 'TVal' the result of applying the given function to all
-- parameter values.
data ParamTrans a = ParamTrans ByteString ([ByteString] -> a)

instance Typeable a => Predicate (ParamTrans a) Request where
    type FVal (ParamTrans a) = Error
    type TVal (ParamTrans a) = a
    apply (ParamTrans x f) r =
        E.lookup ("paramtrans:" <> x) >>= maybe work (return . T D.empty)
      where
        work = case params r x of
            [] -> return (F (err 400 ("Expected parameter '" <> x <> "'.")))
            vs -> do
                let result = f vs
                E.insert ("paramtrans:" <> x) result
                return (T D.empty result)

instance Show (ParamTrans a) where
    show (ParamTrans x _) = "ParamTrans: " ++ show x

-- | ParamGuard is returning as 'TVal' the parameter value which
-- satisfies the given predicate function @ByteString -> Bool@, or
-- else it returns a status of 400 in 'Error'.
data ParamGuard = ParamGuard (ByteString -> Bool) ByteString

instance Predicate ParamGuard Request where
    type FVal ParamGuard = Error
    type TVal ParamGuard = ByteString
    apply (ParamGuard f x) r =
        E.lookup ("paramguard:" <> x) >>= maybe work (return . T D.empty)
      where
        work = case params r x of
            [] -> return (F (err 400 ("Expected parameter '" <> x <> "'.")))
            vs -> maybe failure success (find f vs)

        success p = do
            E.insert ("paramguard:" <> x) p
            return (T D.empty p)

        failure = return (F (err 400 ("Invalid parameter '" <> x <> "'.")))

instance Show ParamGuard where
    show (ParamGuard _ x) = "ParamGuard: " ++ show x
