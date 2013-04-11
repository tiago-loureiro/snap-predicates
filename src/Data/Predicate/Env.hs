{-# LANGUAGE TypeFamilies #-}
module Data.Predicate.Env
  ( Env
  , empty
  , lookup
  , insert
  )
where

import Prelude hiding (lookup)
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Dynamic
import qualified Data.Map.Strict as M

-- | An environment for predicates, consisting of
-- mappings form 'ByteString's to 'Dynamic' values.
newtype Env = Env
  { _unenv :: M.Map ByteString Dynamic }

-- | An empty environment.
empty :: Env
empty = Env M.empty

-- | Try to get the associated value for the given key.
-- Only successful iff, (i) 'Env' contains a binding for 'k'
-- and (ii) the type of value and target match.
lookup :: (MonadState m, StateType m ~ Env, Typeable a) => ByteString -> m (Maybe a)
lookup k = gets $ maybe Nothing fromDynamic . M.lookup k . _unenv

-- | Add a binding from key to value to 'Env', overriding
-- previous bindings if existing.
insert :: (MonadState m, StateType m ~ Env, Typeable a) => ByteString -> a -> m ()
insert k v = modify $ Env . M.insert k (toDyn v) . _unenv
