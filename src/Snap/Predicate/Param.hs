{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Snap.Predicate.Param
  ( Parameter
  , Param
  , ParamOpt
  , ParamDef
  , HasParam

  , parameter
  , param
  , paramOpt
  , paramDef
  , hasParam
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Map (member)
import Data.Monoid
import Data.Predicate.Typeof
import Data.Proxy
import Data.Predicate
import Data.Predicate.Descr hiding (tags)
import Snap.Core
import Snap.Predicate.Error
import Snap.Predicate.Internal
import Snap.Util.Readable

-- | The most generic request parameter predicate provided.
-- It will get all request parameter values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. If the parameter is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data Parameter a = Parameter
    { _pName    :: !ByteString
    , _pRead    :: [ByteString] -> Either ByteString a
    , _pDefault :: Maybe a
    , _pProxy   :: Proxy a
    }

{-# INLINE parameter #-}
parameter :: ByteString
          -- ^ request parameter name
          -> ([ByteString] -> Either ByteString a)
          -- ^ conversion function
          -> Maybe a
          -- ^ optional default value
          -> Parameter a
parameter n r d = Parameter n r d Proxy

instance Predicate (Parameter a) Request where
    type FVal (Parameter a)       = Error
    type TVal (Parameter a)       = a
    apply (Parameter nme f def _) =
        rqApply RqPred
          { _rqName      = nme
          , _rqRead      = f
          , _rqDef       = def
          , _rqCachePref = "parameter:"
          , _rqVals      = params nme
          , _rqError     = Just $ err 400 ("Missing parameter '" <> nme <> "'.")
          }

instance (Typeof a) => Show (Parameter a) where
    show (Parameter n _ _ x) =
        "Parameter: " ++ show n ++ " :: " ++ show (typeof x)

instance (Show a, Typeof a) => Description (Parameter a) where
    describe (Parameter n _ d x) =
        DValue (unpack n) (typeof x) (maybe Required (Default . show) d) tags

-- | Specialisation of 'Parameter' which returns the first request
-- parameter which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
data Param a = Param ByteString (Proxy a)

{-# INLINE param #-}
param :: ByteString -> Param a
param n = Param n Proxy

instance (Readable a) => Predicate (Param a) Request where
    type FVal (Param a) = Error
    type TVal (Param a) = a
    apply (Param x _)   = apply (parameter x readValues Nothing)

instance (Typeof a) => Show (Param a) where
    show (Param n x) = "Param: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (Param a) where
    describe (Param n x) = DValue (unpack n) (typeof x) Required tags

-- | Specialisation of 'Parameter' which returns the first request
-- parameter which could be converted to the target type.
-- If the parameter is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data ParamDef a = ParamDef ByteString a

{-# INLINE paramDef #-}
paramDef :: ByteString -> a -> ParamDef a
paramDef = ParamDef

instance (Readable a) => Predicate (ParamDef a) Request where
    type FVal (ParamDef a) = Error
    type TVal (ParamDef a) = a
    apply (ParamDef x d)   = apply (parameter x readValues (Just d))

instance (Show a, Typeof a) => Show (ParamDef a) where
    show (ParamDef x d) =
        "ParamDef: " ++ show x ++ " [" ++ show d ++ "] :: " ++ show (typeof d)

instance (Show a, Typeof a) => Description (ParamDef a) where
    describe (ParamDef n x) = DValue (unpack n) (typeof x) (Default (show x)) tags

-- | Predicate which returns the first request parameter which could be
-- converted to the target type wrapped in a Maybe.
-- If the parameter is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
data ParamOpt a = ParamOpt ByteString (Proxy a)

{-# INLINE paramOpt #-}
paramOpt :: ByteString -> ParamOpt a
paramOpt n = ParamOpt n Proxy

instance (Readable a) => Predicate (ParamOpt a) Request where
    type FVal (ParamOpt a) = Error
    type TVal (ParamOpt a) = Maybe a
    apply (ParamOpt x _)   =
        rqApplyMaybe RqPred
          { _rqName      = x
          , _rqRead      = readValues
          , _rqDef       = Nothing
          , _rqCachePref = "paramopt:"
          , _rqVals      = params x
          , _rqError     = Nothing
          }

instance (Typeof a) => Show (ParamOpt a) where
    show (ParamOpt n x) = "ParamOpt: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (ParamOpt a) where
    describe (ParamOpt n x) = DValue (unpack n) (typeof x) Optional tags

-- | Predicate which is true if the request has a parameter with the
-- given name.
data HasParam = HasParam ByteString

{-# INLINE hasParam #-}
hasParam :: ByteString -> HasParam
hasParam = HasParam

instance Predicate HasParam Request where
    type FVal HasParam   = Error
    type TVal HasParam   = ()
    apply (HasParam x) r =
        if member x (rqParams r)
            then T 0 ()
            else F (err 400 ("Missing parameter '" <> x <> "'."))

instance Show HasParam where
    show (HasParam x) = "HasParam: " ++ show x

instance Description HasParam where
    describe (HasParam n) = DValue (unpack n) (TPrim PUnit) Required tags

tags :: [Tag]
tags = ["Param"]
