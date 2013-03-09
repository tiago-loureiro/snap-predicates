{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Predicates where

import Control.Applicative hiding (Const)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Word

data Result a =
    Yes !a
  | No !Word !(Maybe ByteString)
  deriving (Eq, Show)

instance Monad Result where
    return = Yes
    (Yes x)  >>= f = f x
    (No i m) >>= _ = No i m

instance MonadPlus Result where
    mzero = No 0 Nothing
    (No _ _) `mplus` y = y
    x        `mplus` _ = x

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    pure  = return
    (<*>) = ap

instance Alternative Result where
    empty = mzero
    (<|>) = mplus

class Predicate a b where
    type Value a
    apply :: a -> b -> Result (Value a)

data Const a = Const a

instance Predicate (Const a) b where
    type Value (Const a) = a
    apply (Const a) _ = Yes a

instance Show a => Show (Const a) where
    show (Const a) = "Const " ++ show a

-- | The logical OR connective of two 'Predicate's.
data a :|: b = a :|: b

instance (Predicate a x, Predicate b x) => Predicate (a :|: b) x where
    type Value (a :|: b) = Either (Value a) (Value b)
    apply (a :|: b) r = (Left <$> apply a r) <|> (Right <$> apply b r)

instance (Show a, Show b) => Show (a :|: b) where
    show (a :|: b) = "(" ++ show a ++ " | " ++ show b ++ ")"

-- | The logical AND connective of two 'Predicate's.
data a :&: b = a :&: b

instance (Predicate a x, Predicate b x) => Predicate (a :&: b) x where
    type Value (a :&: b) = (Value a, Value b)
    apply (a :&: b) r = (,) <$> apply a r <*> apply b r

instance (Show a, Show b) => Show (a :&: b) where
    show (a :&: b) = "(" ++ show a ++ " & " ++ show b ++ ")"
