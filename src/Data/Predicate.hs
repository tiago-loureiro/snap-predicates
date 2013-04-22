{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.Predicate where

import Prelude hiding (and, or)
import Control.Applicative hiding (Const)
import Control.Monad.State.Strict
import Data.Predicate.Delta as Delta
import Data.Predicate.Env (Env)
import qualified Data.Predicate.Env as E

-- | A 'Bool'-like type where each branch 'T'rue or 'F'alse carries
-- some meta-data which is threaded through 'Predicate' evaluation.
data Boolean f t
  = F f       -- ^ logical False with some meta-data
  | T Delta t -- ^ logical True with some meta-data
  deriving (Eq, Show)

-- | The 'Predicate' class declares the function 'apply' which
-- evaluates the predicate against some value, returning a value
-- of type 'Boolean'.
-- Besides being parameterised over predicate type and predicate
-- parameter, the class is also parameterised over the actual types
-- of T's and F's meta-data.
class Predicate p a where
    type FVal p
    type TVal p
    apply :: p -> a -> State Env (Boolean (FVal p) (TVal p))

-- | A 'Predicate' instance which always returns 'T' with
-- the given value as T's meta-data.
data Const f t where
    Const :: t -> Const f t

instance Predicate (Const f t) a where
    type FVal (Const f t) = f
    type TVal (Const f t) = t
    apply (Const a) _     = return (T Delta.empty a)

instance Show t => Show (Const f t) where
    show (Const a) = "Const " ++ show a

-- | A 'Predicate' instance which always returns 'F' with
-- the given value as F's meta-data.
data Fail f t where
    Fail :: f -> Fail f t

instance Predicate (Fail f t) a where
    type FVal (Fail f t) = f
    type TVal (Fail f t) = t
    apply (Fail a) _     = return (F a)

instance Show f => Show (Fail f t) where
    show (Fail a) = "Fail " ++ show a

-- | A 'Predicate' instance corresponding to the logical
-- OR connective of two 'Predicate's. It requires the
-- meta-data of each 'T'rue branch to be of the same type.
--
-- If both arguments evaluate to 'T' the one with the
-- smaller 'Delta' will be preferred, or--if equal--the
-- left-hand argument.
data a :|: b = a :|: b

instance (Predicate a c, Predicate b c, TVal a ~ TVal b, FVal a ~ FVal b) => Predicate (a :|: b) c
  where
    type FVal (a :|: b) = FVal a
    type TVal (a :|: b) = TVal a
    apply (a :|: b) r   = or <$> apply a r <*> apply b r
      where
        or x@(T d0 _) y@(T d1 _) = if d1 < d0 then y else x
        or x@(T _  _)   (F    _) = x
        or (F      _) x@(T _  _) = x
        or (F      _) x@(F    _) = x

instance (Show a, Show b) => Show (a :|: b) where
    show (a :|: b) = "(" ++ show a ++ " | " ++ show b ++ ")"

type a :+: b = Either a b

-- | A 'Predicate' instance corresponding to the logical
-- OR connective of two 'Predicate's. The meta-data of
-- each 'T'rue branch can be of different types.
--
-- If both arguments evaluate to 'T' the one with the
-- smaller 'Delta' will be preferred, or--if equal--the
-- left-hand argument.
data a :||: b = a :||: b

instance (Predicate a c, Predicate b c, FVal a ~ FVal b) => Predicate (a :||: b) c
  where
    type FVal (a :||: b) = FVal a
    type TVal (a :||: b) = TVal a :+: TVal b
    apply (a :||: b) r   = or <$> apply a r <*> apply b r
      where
        or (T d0 t0) (T d1 t1) = if d1 < d0 then T d1 (Right t1) else T d0 (Left t0)
        or (T  d  t) (F     _) = T d (Left t)
        or (F     _) (T  d  t) = T d (Right t)
        or (F     _) (F     f) = F f

instance (Show a, Show b) => Show (a :||: b) where
    show (a :||: b) = "(" ++ show a ++ " || " ++ show b ++ ")"

-- | Data-type used for tupling-up the results of ':&:'.
data a :*: b = a :*: b deriving (Eq, Show)

-- | A 'Predicate' instance corresponding to the logical
-- AND connective of two 'Predicate's.
data a :&: b = a :&: b

instance (Predicate a c, Predicate b c, FVal a ~ FVal b) => Predicate (a :&: b) c
  where
    type FVal (a :&: b) = FVal a
    type TVal (a :&: b) = TVal a :*: TVal b
    apply (a :&: b) r   = and <$> apply a r <*> apply b r
      where
        and (T d x) (T w y) = T (d +++ w) (x :*: y)
        and (T _ _) (F   f) = F f
        and (F   f) _       = F f

instance (Show a, Show b) => Show (a :&: b) where
    show (a :&: b) = "(" ++ show a ++ " & " ++ show b ++ ")"

-- | Evaluate the given predicate 'p' against the given value 'a'.
eval :: Predicate p a => p -> a -> Boolean (FVal p) (TVal p)
eval p a = evalState (apply p a) E.empty

-- | The 'with' function will invoke the given function only if the predicate 'p'
-- applied to the test value 'a' evaluates to 'T'.
with :: (Monad m, Predicate p a) => p -> a -> (TVal p -> m ()) -> m ()
with p a f =
    case eval p a of
        T _ x -> f x
        _     -> return ()
