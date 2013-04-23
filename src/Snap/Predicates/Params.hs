{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates.Params
  ( Parameter (..)
  , Param (..)
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

-- | The most generic request parameter predicate provided.
-- It will get all request parameter values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. In case of error, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data Parameter a = Parameter
  { _name    :: !ByteString                         -- ^ request parameter name
  , _read    :: [ByteString] -> Either ByteString a -- ^ conversion function
  , _default :: !(Maybe a)                          -- ^ (optional) default value
  }

instance Typeable a => Predicate (Parameter a) Request where
    type FVal (Parameter a) = Error
    type TVal (Parameter a) = a
    apply (Parameter nme f def) r =
        E.lookup (key nme) >>= maybe work result
      where
        work = case params r nme of
            [] -> return (F (err 400 ("Missing parameter '" <> nme <> "'.")))
            vs -> do
                let x = f vs
                E.insert (key nme) x
                case x of
                    Left msg -> return $ maybe (F (err 400 msg)) (T D.empty) def
                    Right  v -> return $ T D.empty v

        result (Left msg) = return (F (err 400 msg))
        result (Right  v) = return (T D.empty v)

        key = ("parameter:" <>)

instance Show (Parameter a) where
    show p = "Parameter: " ++ show (_name p)

-- | Specialisation of 'Parameter' which returns the first request
-- parameter value as is.
data Param = Param ByteString deriving Eq

instance Predicate Param Request where
    type FVal Param = Error
    type TVal Param = ByteString
    apply (Param x) = apply (Parameter x (Right . head) Nothing)

instance Show Param where
    show (Param x) = "Param: " ++ show x

-- | Specialisation of 'Parameter' which returns the first request
-- parameter satisfying the provided predicate function as is.
data ParamGuard = ParamGuard ByteString (ByteString -> Bool)

instance Predicate ParamGuard Request where
    type FVal ParamGuard   = Error
    type TVal ParamGuard   = ByteString
    apply (ParamGuard x f) = apply (Parameter x fun Nothing)
      where
        fun vs = case find f vs of
           Nothing -> Left ("Invalid parameter: '" <> x <> "'.")
           Just  v -> Right v

instance Show ParamGuard where
    show (ParamGuard x _) = "ParamGuard: " ++ show x
