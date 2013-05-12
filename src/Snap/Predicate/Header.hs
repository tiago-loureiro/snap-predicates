{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicate.Header
  ( Header (..)
  , Hdr    (..)
  , HdrOpt (..)
  , HdrDef (..)
  , HasHdr (..)
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Readable
import Data.CaseInsensitive (mk)
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Predicate
import Snap.Core hiding (headers)
import Snap.Predicate.Error
import Snap.Predicate.Internal

-- | The most generic request header predicate provided.
-- It will get all request header values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. If the header is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data Header a = Header
  { _hdrName    :: !ByteString                         -- ^ request header name
  , _hdrRead    :: [ByteString] -> Either ByteString a -- ^ conversion function
  , _hdrDefault :: !(Maybe a)                          -- ^ (optional) default value
  }

instance Typeable a => Predicate (Header a) Request where
    type FVal (Header a)     = Error
    type TVal (Header a)     = a
    apply (Header nme f def) =
        rqApply RqPred
          { _rqName      = nme
          , _rqRead      = f
          , _rqDef       = def
          , _rqCachePref = "header:"
          , _rqVals      = headers nme
          , _rqError     = Just $ err 400 ("Missing header '" <> nme <> "'.")
          }

instance Show (Header a) where
    show p = "Header: " ++ show (_hdrName p)

-- | Specialisation of 'Header' which returns the first request
-- header value which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
data Hdr a = Hdr ByteString

instance (Typeable a, Readable a) => Predicate (Hdr a) Request where
    type FVal (Hdr a) = Error
    type TVal (Hdr a) = a
    apply (Hdr x)     = apply (Header x readValues Nothing)

instance Show (Hdr a) where
    show (Hdr x) = "Hdr: " ++ show x

-- | Specialisation of 'Header' which returns the first request
-- header value which could be converted to the target type.
-- If the header is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data HdrDef a = HdrDef ByteString a

instance (Typeable a, Readable a) => Predicate (HdrDef a) Request where
    type FVal (HdrDef a) = Error
    type TVal (HdrDef a) = a
    apply (HdrDef x d)   = apply (Header x readValues (Just d))

instance Show a => Show (HdrDef a) where
    show (HdrDef x d) = "HdrDef: " ++ show x ++ " [" ++ show d ++ "]"

-- | Predicate which returns the first request header which could be
-- converted to the target type wrapped in a Maybe.
-- If the header is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
data HdrOpt a = HdrOpt ByteString

instance (Typeable a, Readable a) => Predicate (HdrOpt a) Request where
    type FVal (HdrOpt a) = Error
    type TVal (HdrOpt a) = Maybe a
    apply (HdrOpt x)     =
        rqApplyMaybe RqPred
          { _rqName      = x
          , _rqRead      = readValues
          , _rqDef       = Nothing
          , _rqCachePref = "headeropt:"
          , _rqVals      = headers x
          , _rqError     = Nothing
          }

instance Show (HdrOpt a) where
    show (HdrOpt x) = "HdrOpt: " ++ show x

-- | Predicate which is true if the request has a header with the
-- given name.
data HasHdr = HasHdr ByteString

instance Predicate HasHdr Request where
    type FVal HasHdr   = Error
    type TVal HasHdr   = ()
    apply (HasHdr x) r = return $
        if isJust (getHeaders (mk x) r)
            then T 0 ()
            else F (err 400 ("Missing header '" <> x <> "'."))

instance Show HasHdr where
    show (HasHdr x) = "HasHdr: " ++ show x
