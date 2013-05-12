{-# LANGUAGE BangPatterns #-}
module Snap.Predicate.Error where

import Data.ByteString (ByteString)
import Data.Word

-- | The error type used as 'F' meta-data in all snap predicates.
data Error = Error
  { _status  :: !Word               -- ^ (HTTP) status code
  , _message :: !(Maybe ByteString) -- ^ optional status message
  } deriving (Eq, Show)

-- | Convenience function to construct 'Error' values from
-- status code and status message.
err :: Word -> ByteString -> Error
err s = Error s . Just
