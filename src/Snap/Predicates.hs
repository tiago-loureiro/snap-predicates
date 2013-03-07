{-# LANGUAGE OverloadedStrings, BangPatterns, TypeOperators, GADTs #-}
module Snap.Predicates
  ( Predicate (..)
  , Result (..)
  , Anything (..)
  , Accept (..)
  , AnyParamOf (..)
  , AnyHeaderOf (..)
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
data Result =
    Good
  | Bad !Word !(Maybe ByteString)
  deriving (Eq, Show)

-- | The predicate type-class contains the abstract
-- interface to evaluate a predicate against some
-- 'Request' and the ability to turn a predicate
-- into a string representation.
class Predicate a where
    apply :: a -> Request -> Result
    toStr :: a -> ByteString

-- | A 'Predicate' instance which is always 'Good'.
data Anything = Anything

-- | The logical OR connective of two 'Predicate's.
data a :|: b where
    (:|:) :: (Predicate a, Predicate b) => a -> b -> a :|: b

-- | The logical AND connective of two 'Predicate's.
data a :&: b where
    (:&:) :: (Predicate a, Predicate b) => a -> b -> a :&: b

instance Predicate Anything where
    apply Anything _ = Good
    toStr Anything   = "Anything"

instance Predicate (a :|: b) where
    apply (a :|: b) r =
        case apply a r of
            Good -> Good
            _    -> apply b r
    toStr (a :|: b) = toStr a <> " | " <> toStr b

instance Predicate (a :&: b) where
    apply (a :&: b) r =
        case apply a r of
            Good -> apply b r
            bad  -> bad
    toStr (a :&: b) = toStr a <> " & " <> toStr b

-- | A 'Predicate' against the 'Request's "Accept" header.
data Accept      = Accept ByteString
                 --
-- | A 'Predicate' which tests if any of the given
-- names denote a 'Request' parameter.
data AnyParamOf  = AnyParamOf [ByteString]

-- | A 'Predicate' which tests if any of the given
-- names denote a 'Request' header name.
data AnyHeaderOf = AnyHeaderOf [ByteString]

instance Predicate Accept where
    apply (Accept x) r =
        if x `elem` headers' r "accept"
            then Good
            else Bad 406 (Just "Expected 'Accept: accept/json'.")
    toStr (Accept x) = "Accept: " <> show' x

instance Predicate AnyParamOf where
    apply (AnyParamOf xs) r =
        if null . concat $ map (params' r) xs
            then Bad 400 (Just ("Expected any of " <> show' xs <> "."))
            else Good
    toStr (AnyParamOf xs) = "AnyParamOf: " <> show' xs

-- | Evaluates a 'Predicate' against the Snap 'Request'.
eval :: (MonadSnap m, Predicate p) => p -> m Result
eval p = apply p <$> getRequest

-- Internal helpers:

headers' :: Request -> ByteString -> [ByteString]
headers' rq name = maybe [] id . getHeaders (CI.mk name) $ rq

params' :: Request -> ByteString -> [ByteString]
params' rq name = maybe [] id . M.lookup name . rqParams $ rq

show' :: Show a => a -> ByteString
show' = fromString . show
