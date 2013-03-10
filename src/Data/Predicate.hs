{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
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
-- parameter, the class leaves open the actual types of 'T's and
-- 'F's meta-data, which must be defined in instance declarations.
class Predicate a b where
    type FVal a
    type TVal a
    apply :: a -> b -> Boolean (FVal a) (TVal a)

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
data Const f t where
    Const :: t -> Const f t

instance Predicate (Const f t) c where
    type FVal (Const f t) = f
    type TVal (Const f t) = t
    apply (Const a) _     = T a

instance Show t => Show (Const f t) where
    show (Const a) = "Const " ++ show a

-- | A 'Predicate' instance which always returns 'F' with
-- the given 'FVal' as meta-data.
data Fail f t where
    Fail :: f -> Fail f t

instance Predicate (Fail f t) c where
    type FVal (Fail f t) = f
    type TVal (Fail f t) = t
    apply (Fail a) _     = F $ Just a

instance Show f => Show (Fail f t) where
    show (Fail a) = "Fail " ++ show a

-- | A 'Predicate' instance corresponding to the logical
-- OR connective of two 'Predicate's.
data a :|: b = a :|: b

instance (Predicate a x, Predicate b x, FVal a ~ FVal b) => Predicate (a :|: b) x
  where
    type FVal (a :|: b) = FVal a
    type TVal (a :|: b) = Either (TVal a) (TVal b)
    apply (a :|: b) r   = (Left <$> apply a r) <|> (Right <$> apply b r)

instance (Show a, Show b) => Show (a :|: b) where
    show (a :|: b) = "(" ++ show a ++ " | " ++ show b ++ ")"

-- | A 'Predicate' instance corresponding to the logical
-- AND connective of two 'Predicate's.
data a :&: b = a :&: b

instance (Predicate a x, Predicate b x, FVal a ~ FVal b) => Predicate (a :&: b) x
  where
    type FVal (a :&: b) = FVal a
    type TVal (a :&: b) = (TVal a, TVal b)
    apply (a :&: b) r   = (,) <$> apply a r <*> apply b r

instance (Show a, Show b) => Show (a :&: b) where
    show (a :&: b) = "(" ++ show a ++ " & " ++ show b ++ ")"
