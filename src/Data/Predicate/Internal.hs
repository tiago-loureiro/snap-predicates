module Data.Predicate.Internal where

import Data.Proxy
import Data.Typeable

{-# INLINE typeRepOf #-}
typeRepOf :: Typeable a => Proxy a -> TypeRep
typeRepOf p = head $ typeRepArgs (typeOf p)
