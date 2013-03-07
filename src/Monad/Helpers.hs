module Monad.Helpers where

import Control.Applicative

ifM :: (Functor m, Monad m) => m Bool -> a -> a -> m a
ifM t x y = (\b -> if b then x else y) <$> t
