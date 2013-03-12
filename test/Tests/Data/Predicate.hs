{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Tests.Data.Predicate (tests) where

import Control.Applicative hiding (Const)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Predicate

tests :: [Test]
tests =
    [ testProperty "Const" testConst
    , testProperty "Fail" testFail
    , testProperty "(:&:)" testAnd
    , testProperty "(:||:)" testOr
    , testProperty "(:|:)" testOr'
    ]

testConst :: Const Int Char -> Bool
testConst x@(Const c) = apply x () == T c

testFail :: Fail Int Char -> Bool
testFail x@(Fail c) = apply x () == F (Just c)

testAnd :: Rand -> Rand -> Bool
testAnd a@(Rand (T x)) b@(Rand (T y)) = apply (a :&: b) () == T (x :*: y)
testAnd a@(Rand (T _)) b@(Rand (F y)) = apply (a :&: b) () == F y
testAnd a@(Rand (F x)) b@(Rand (T _)) = apply (a :&: b) () == F x
testAnd a@(Rand (F x)) b@(Rand (F _)) = apply (a :&: b) () == F x

testOr :: Rand -> Rand -> Bool
testOr a@(Rand (T x)) b@(Rand (T _)) = apply (a :||: b) () == T (Left x)
testOr a@(Rand (T x)) b@(Rand (F _)) = apply (a :||: b) () == T (Left x)
testOr a@(Rand (F _)) b@(Rand (T y)) = apply (a :||: b) () == T (Right y)
testOr a@(Rand (F _)) b@(Rand (F y)) = apply (a :||: b) () == F y

testOr' :: Rand -> Rand -> Bool
testOr' a@(Rand (T x)) b@(Rand (T _)) = apply (a :|: b) () == T x
testOr' a@(Rand (T x)) b@(Rand (F _)) = apply (a :|: b) () == T x
testOr' a@(Rand (F _)) b@(Rand (T y)) = apply (a :|: b) () == T y
testOr' a@(Rand (F _)) b@(Rand (F y)) = apply (a :|: b) () == F y

data Rand = Rand (Boolean Int Char) deriving Show

instance Predicate Rand a where
    type FVal Rand   = Int
    type TVal Rand   = Char
    apply (Rand x) _ = x

instance Arbitrary (Boolean Int Char) where
    arbitrary =
        oneof [ T <$> (arbitrary :: Gen Char)
              , F <$> (arbitrary :: Gen (Maybe Int))
              ]

instance Arbitrary (Const Int Char) where
    arbitrary = Const <$> (arbitrary :: Gen Char)

instance Arbitrary (Fail Int Char) where
    arbitrary = Fail <$> (arbitrary :: Gen Int)

instance Arbitrary Rand where
    arbitrary = Rand <$> (arbitrary :: Gen (Boolean Int Char))
