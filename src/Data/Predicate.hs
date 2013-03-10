{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.Predicate where

import Control.Applicative hiding (Const)
import Control.Monad

-- | A 'Bool'-like type where each branch 'T'rue or 'F'alse carries
-- some meta-data which are threaded through 'Predicate' evaluation.
data Boolean f t =
    F (Maybe f) -- ^ logical False with some meta-data
  | T t         -- ^ logical True with some meta-data
  deriving (Eq, Show)

-- | The 'Predicate' class declares the functions 'apply' which
-- evaluates the predicate against some value, returning a value
-- of type 'Boolean'.
--
-- Besides being parameterised over predicate type and predicate
-- parameter, the class is also parameterised over the actual types
-- of 'T's and 'F's meta-data.
class Predicate p a f t where
    apply :: p -> a -> Boolean f t

instance Monad (Boolean f) where
    return      = T
    (T x) >>= g = g x
    (F x) >>= _ = F x

instance MonadPlus (Boolean f) where
    mzero           = F Nothing
    (F _) `mplus` b = b
    b     `mplus` _ = b

instance Functor (Boolean f) where
    fmap = liftM

instance Applicative (Boolean f) where
    pure  = return
    (<*>) = ap

instance Alternative (Boolean f) where
    empty = mzero
    (<|>) = mplus

-- | A 'Predicate' instance which always returns 'T' with
-- the given 'TVal' as meta-data.
data Const t = Const t

instance Predicate (Const t) a f t where
    apply (Const a) _ = T a

instance Show t => Show (Const t) where
    show (Const a) = "Const " ++ show a

-- | A 'Predicate' instance which always returns 'F' with
-- the given 'FVal' as meta-data.
data Fail f = Fail f

instance Predicate (Fail f) a f t where
    apply (Fail a) _ = F $ Just a

instance Show f => Show (Fail f) where
    show (Fail a) = "Fail " ++ show a

-- | A 'Predicate' instance corresponding to the logical
-- OR connective of two 'Predicate's.
data a :|: b = a :|: b

instance (Predicate a c f t0, Predicate b c f t1) =>
    Predicate (a :|: b) c f (Either t0 t1)
  where
    apply (a :|: b) r = (Left <$> apply a r) <|> (Right <$> apply b r)

instance (Show a, Show b) => Show (a :|: b) where
    show (a :|: b) = "(" ++ show a ++ " | " ++ show b ++ ")"

-- | A 'Predicate' instance corresponding to the logical
-- AND connective of two 'Predicate's.
data a :&: b = a :&: b

instance (Predicate a c f t0, Predicate b c f t1) =>
    Predicate (a :&: b) c f (t0, t1)
  where
    apply (a :&: b) r = (,) <$> apply a r <*> apply b r

instance (Show a, Show b) => Show (a :&: b) where
    show (a :&: b) = "(" ++ show a ++ " & " ++ show b ++ ")"
