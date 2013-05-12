{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicate.Param
  ( Parameter (..)
  , Param     (..)
  , ParamOpt  (..)
  , ParamDef  (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Readable
import Data.Monoid
import Data.Typeable
import Data.Predicate
import Snap.Core
import Snap.Predicate.Error
import Snap.Predicate.Internal

-- | The most generic request parameter predicate provided.
-- It will get all request parameter values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. If the parameter is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data Parameter a = Parameter
  { _pName    :: !ByteString                         -- ^ request parameter name
  , _pRead    :: [ByteString] -> Either ByteString a -- ^ conversion function
  , _pDefault :: !(Maybe a)                          -- ^ (optional) default value
  }

instance Typeable a => Predicate (Parameter a) Request where
    type FVal (Parameter a)     = Error
    type TVal (Parameter a)     = a
    apply (Parameter nme f def) =
        rqApply RqPred
          { _rqName      = nme
          , _rqRead      = f
          , _rqDef       = def
          , _rqCachePref = "parameter:"
          , _rqVals      = params nme
          , _rqError     = Just $ err 400 ("Missing parameter '" <> nme <> "'.")
          }

instance Show (Parameter a) where
    show p = "Parameter: " ++ show (_pName p)

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
-- If the parameter is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
data ParamOpt a = ParamOpt ByteString

instance (Typeable a, Readable a) => Predicate (ParamOpt a) Request where
    type FVal (ParamOpt a) = Error
    type TVal (ParamOpt a) = Maybe a
    apply (ParamOpt x)     =
        rqApplyMaybe RqPred
          { _rqName      = x
          , _rqRead      = readValues
          , _rqDef       = Nothing
          , _rqCachePref = "paramopt:"
          , _rqVals      = params x
          , _rqError     = Nothing
          }

instance Show (ParamOpt a) where
    show (ParamOpt x) = "ParamOpt: " ++ show x
