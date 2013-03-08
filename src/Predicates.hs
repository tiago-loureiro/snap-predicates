{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Predicates where

import Prelude hiding (True, False)
import Control.Applicative
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

-- | The logical True.
data True = True

instance Predicate True a where
    type Value True = ()
    apply True _ = Yes ()

instance Show True where
    show True = "True"

-- | The logical False.
data False = False

instance Predicate False a where
    type Value False = ()
    apply False _ = No 0 Nothing

instance Show False where
    show False = "False"

-- | The logical NOT connective of a 'Predicate'.
data Not a = Not a

instance Predicate a x => Predicate (Not a) x where
    type Value (Not a) = ()
    apply (Not a) r =
        case apply a r of
            Yes _ -> No 0 Nothing
            _      -> Yes ()

instance Show a => Show (Not a) where
    show (Not a) = "(not " ++ show a ++ ")"

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
