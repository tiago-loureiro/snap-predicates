{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates.Params
  ( Parameter (..)
  , Param (..)
  , ParamOpt (..)
  , ParamDef (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Readable
import Data.Monoid
import Data.String
import Data.Typeable
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicates.Error
import Snap.Predicates.Internal
import qualified Data.Predicate.Env as E

-- | The most generic request parameter predicate provided.
-- It will get all request parameter values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. If the parameter is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data Parameter a = Parameter
  { _name    :: !ByteString                         -- ^ request parameter name
  , _read    :: [ByteString] -> Either ByteString a -- ^ conversion function
  , _default :: !(Maybe a)                          -- ^ (optional) default value
  }

instance Typeable a => Predicate (Parameter a) Request where
    type FVal (Parameter a)       = Error
    type TVal (Parameter a)       = a
    apply (Parameter nme f def) r =
        let k = key "parameter:" nme def
        in E.lookup k >>= maybe (work k) result
      where
        work k = case params r nme of
            [] -> maybe (return (F (err 400 ("Missing parameter '" <> nme <> "'."))))
                        (return . (T 0))
                        def
            vs -> do
                let v = f vs
                E.insert k v
                result v

        result = return . either (F . err 400) (T 0)

instance Show (Parameter a) where
    show p = "Parameter: " ++ show (_name p)

-- | Specialisation of 'Parameter' which returns the first request
-- parameter which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
data Param a = Param ByteString

instance (Typeable a, Readable a) => Predicate (Param a) Request where
    type FVal (Param a) = Error
    type TVal (Param a) = a
    apply (Param x)     = apply (Parameter x readValues Nothing)

instance Show (Param a) where
    show (Param x) = "Param: " ++ show x

-- | Specialisation of 'Parameter' which returns the first request
-- parameter which could be converted to the target type.
-- If the parameter is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data ParamDef a = ParamDef ByteString a

instance (Typeable a, Readable a) => Predicate (ParamDef a) Request where
    type FVal (ParamDef a) = Error
    type TVal (ParamDef a) = a
    apply (ParamDef x d)   = apply (Parameter x readValues (Just d))

instance Show a => Show (ParamDef a) where
    show (ParamDef x d) = "ParamDef: " ++ show x ++ " [" ++ show d ++ "]"

-- | Predicate which returns the first request parameter which could be
-- converted to the target type wrapped in a Maybe.
-- If the parameter is not present, the 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
data ParamOpt a = ParamOpt ByteString

instance (Typeable a, Readable a) => Predicate (ParamOpt a) Request where
    type FVal (ParamOpt a) = Error
    type TVal (ParamOpt a) = Maybe a
    apply (ParamOpt x) r   =
        let n = Nothing :: (Typeable a, Readable a) => Maybe a
            k = key "paramopt:" x n
        in E.lookup k >>= maybe (work k n) result
      where
        work k n = case params r x of
            []     -> return (T 0 n)
            values -> do
                let v = readValues values
                E.insert k v
                result v

        result = return . either (F . err 400) (T 0 . Just)

key :: (Typeable a, IsString m, Monoid m) => m -> m -> a -> m
key prefix name def = prefix <> name <> ":" <> (fromString . show . typeOf $ def)

instance Show (ParamOpt a) where
    show (ParamOpt x) = "ParamOpt: " ++ show x
