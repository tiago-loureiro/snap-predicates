{-# LANGUAGE OverloadedStrings, BangPatterns, TypeOperators, GADTs, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Snap.Predicates
  ( Predicate (..)
  , Result (..)
  , Anything (..)
  , Accept (..)
  , Param (..)
  , (:&:) (..)
  , (:|:) (..)
  , eval
  )
where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Monoid
import Data.String
import Data.Word
import Snap.Core
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as M

-- | The result of some predicate evaluation,
-- either 'Good' or 'Bad' where the latter includes some
-- status code plus optional message.
data Result a =
    Good a
  | Bad !Word !(Maybe ByteString)
  deriving (Eq, Show)

-- | The predicate type-class contains the abstract
-- interface to evaluate a predicate against some
-- 'Request' and the ability to turn a predicate
-- into a string representation.
class Predicate a b | a -> b where
    apply :: a -> Request -> Result b
    toStr :: a -> ByteString

-- | A 'Predicate' instance which is always 'Good'.
data Anything = Anything

-- | The logical OR connective of two 'Predicate's.
data a :|: b where
    (:|:) :: a -> b -> a :|: b

-- | The logical AND connective of two 'Predicate's.
data a :&: b where
    (:&:) :: a -> b -> a :&: b

instance Predicate Anything () where
    apply Anything _  = Good ()
    toStr Anything    = "Anything"

instance (Predicate a x, Predicate b y) => Predicate (a :|: b) (Either x y) where
    apply (a :|: b) r =
        case apply a r of
            Good x -> Good (Left x)
            _      -> case apply b r of
                          Good y  -> Good (Right y)
                          Bad i m -> Bad i m
    toStr (a :|: b) = toStr a <> " | " <> toStr b

instance (Predicate a x, Predicate b y) => Predicate (a :&: b) (x, y) where
    apply (a :&: b) r =
        case apply a r of
            Good x  -> case apply b r of
                           Good y  -> Good (x, y)
                           Bad i m -> Bad i m
            Bad i m -> Bad i m
    toStr (a :&: b) = toStr a <> " & " <> toStr b

-- | A 'Predicate' against the 'Request's "Accept" header.
data Accept = Accept ByteString

-- | A 'Predicate' looking for some parameter value.
data Param  = Param ByteString

instance Predicate Accept () where
    apply (Accept x) r =
        if x `elem` headers' r "accept"
            then Good ()
            else Bad 406 (Just "Expected 'Accept: accept/json'.")
    toStr (Accept x) = "Accept: " <> show' x

instance Predicate Param ByteString where
    apply (Param x) r =
        case params' r x of
            []    -> Bad 400 (Just ("Expected parameter " <> show' x <> "."))
            (v:_) -> Good v
    toStr (Param x) = "Param: " <> show' x

-- | Evaluates a 'Predicate' against the Snap 'Request'.
eval :: (MonadSnap m, Predicate p x) => p -> m (Result x)
eval p = apply p <$> getRequest

-- Internal helpers:

headers' :: Request -> ByteString -> [ByteString]
headers' rq name = maybe [] id . getHeaders (CI.mk name) $ rq

params' :: Request -> ByteString -> [ByteString]
params' rq name = maybe [] id . M.lookup name . rqParams $ rq

show' :: Show a => a -> ByteString
show' = fromString . show
