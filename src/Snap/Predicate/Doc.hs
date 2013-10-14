{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Snap.Predicate.Doc
    ( Doc
    , doc
    ) where

import Data.Predicate
import Data.Predicate.Descr

data Doc = Doc String

doc :: String -> Doc
doc = Doc

instance Predicate Doc b where
    type FVal Doc = ()
    type TVal Doc = ()
    apply _ _ = T 0 ()

instance Show Doc where
    show (Doc d) = d

instance Description Doc where
    describe (Doc d) = DDoc d
