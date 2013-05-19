{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Snap.Predicate.Types ( CSV, list ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Snap.Util.Readable
import qualified Data.ByteString.Char8 as C

newtype CSV a = CSV { list :: [a] } deriving
  ( Eq
  , Ord
  , Read
  , Show
  , Functor
  , Monad
  , MonadPlus
  , Applicative
  , Alternative
  , Monoid
  , Typeable
  )

instance Readable a => Readable (CSV a) where
    fromBS s
      | C.null s  = return empty
      | otherwise =
          let cs = map (fromBS . trim) (C.split ',' s)
              xs = catMaybes cs
          in if length cs /= length xs
                 then fail "no parse"
                 else return (CSV xs)

trim :: ByteString -> ByteString
trim = fst . C.spanEnd (== ' ') . C.dropWhile (== ' ')
