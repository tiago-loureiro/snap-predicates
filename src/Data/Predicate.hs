{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.Predicate where

import Prelude hiding (and, or)
import Control.Applicative hiding (Const)
import Control.Monad.State.Strict
import Data.Predicate.Env (Env)
import qualified Data.Predicate.Env as E

-- | A 'Bool'-like type where each branch 'T'rue or 'F'alse carries
-- some meta-data which is threaded through 'Predicate' evaluation.
data Boolean f t
  = F f       -- ^ logical False with some meta-data
  | T Delta t -- ^ logical True with some meta-data
  deriving (Eq, Show)

-- | 'Delta' is a measure of distance. It is (optionally)
-- used in predicates that evaluate to 'T' but not uniquely so, i.e.
-- different evaluations of 'T' are possible and they may differ in how
-- close they are to the ideal @T []@. The range of each 'Double' value
-- is [0.0, 1.0].
--
-- An example is content-negotiation. A HTTP request may specify
-- a preference list of various media-types. A predicate matching one
-- specific media-type evaluates to 'T', but other media-types may match
-- even better. To represent this ambivalence, the predicate will include
-- a delta value which can be used to decide which of the matching
-- predicates should be preferred.
type Delta = [Double]

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
    apply (Const a) _     = return (T [] a)

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
data a :|: b = a :|: b

instance (Predicate a c, Predicate b c, TVal a ~ TVal b, FVal a ~ FVal b) => Predicate (a :|: b) c
  where
    type FVal (a :|: b) = FVal a
    type TVal (a :|: b) = TVal a
    apply (a :|: b) r   = or <$> apply a r <*> apply b r
      where
        or (T d t) _       = T d t
        or (F   _) (T d t) = T d t
        or (F   _) (F   f) = F f

instance (Show a, Show b) => Show (a :|: b) where
    show (a :|: b) = "(" ++ show a ++ " | " ++ show b ++ ")"

type a :+: b = Either a b

-- | A 'Predicate' instance corresponding to the logical
-- OR connective of two 'Predicate's. The meta-data of
-- each 'T'rue branch can be of different types.
data a :||: b = a :||: b

instance (Predicate a c, Predicate b c, FVal a ~ FVal b) => Predicate (a :||: b) c
  where
    type FVal (a :||: b) = FVal a
    type TVal (a :||: b) = TVal a :+: TVal b
    apply (a :||: b) r   = or <$> apply a r <*> apply b r
      where
        or (T d t) _       = T d (Left  t)
        or (F   _) (T d t) = T d (Right t)
        or (F   _) (F   f) = F f

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
        and (T d x) (T w y) = T (d ++ w) (x :*: y)
        and (T _ _) (F   f) = F f
        and (F   f) _       = F f

instance (Show a, Show b) => Show (a :&: b) where
    show (a :&: b) = "(" ++ show a ++ " & " ++ show b ++ ")"

-- | The 'with' function will invoke the given function only if the predicate 'p'
-- applied to the test value 'a' evaluates to 'T'.
with :: (Monad m, Predicate p a) => p -> a -> (TVal p -> m ()) -> m ()
with p a f =
    case evalState (apply p a) E.empty of
        T _ x -> f x
        _     -> return ()
