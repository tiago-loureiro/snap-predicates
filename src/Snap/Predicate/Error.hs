{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Snap.Predicate.Error where

import Data.ByteString (ByteString)
import Data.Predicate.Typeof
import Data.Word
import GHC.Generics

-- | The error type used as 'F' meta-data in all snap predicates.
data Error = Error
    { status  :: !Word               -- ^ (HTTP) status code
    , message :: !(Maybe ByteString) -- ^ optional status message
    } deriving (Eq, Show, Generic)

instance Typeof Error

-- | Convenience function to construct 'Error' values from
-- status code and status message.
err :: Word -> ByteString -> Error
err s = Error s . Just
