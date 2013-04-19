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
testConst x@(Const c) = eval x () == T [] c

testFail :: Fail Int Char -> Bool
testFail x@(Fail c) = eval x () == F c

testAnd :: Rand -> Rand -> Bool
testAnd a@(Rand (T d x)) b@(Rand (T w y)) = eval (a :&: b) () == T (d ++ w) (x :*: y)
testAnd a@(Rand (T _ _)) b@(Rand (F   y)) = eval (a :&: b) () == F y
testAnd a@(Rand (F   x)) b@(Rand (T _ _)) = eval (a :&: b) () == F x
testAnd a@(Rand (F   x)) b@(Rand (F   _)) = eval (a :&: b) () == F x

testOr :: Rand -> Rand -> Bool
testOr a@(Rand (T d x)) b@(Rand (T _ _)) = eval (a :||: b) () == T d (Left x)
testOr a@(Rand (T d x)) b@(Rand (F   _)) = eval (a :||: b) () == T d (Left x)
testOr a@(Rand (F   _)) b@(Rand (T d y)) = eval (a :||: b) () == T d (Right y)
testOr a@(Rand (F   _)) b@(Rand (F   y)) = eval (a :||: b) () == F y

testOr' :: Rand -> Rand -> Bool
testOr' a@(Rand (T d x)) b@(Rand (T _ _)) = eval (a :|: b) () == T d x
testOr' a@(Rand (T d x)) b@(Rand (F   _)) = eval (a :|: b) () == T d x
testOr' a@(Rand (F   _)) b@(Rand (T d y)) = eval (a :|: b) () == T d y
testOr' a@(Rand (F   _)) b@(Rand (F   y)) = eval (a :|: b) () == F y

data Rand = Rand (Boolean Int Char) deriving Show

instance Predicate Rand a where
    type FVal Rand   = Int
    type TVal Rand   = Char
    apply (Rand x) _ = return x

instance Arbitrary (Boolean Int Char) where
    arbitrary =
        oneof [ T <$> (arbitrary :: Gen Delta) <*> (arbitrary :: Gen Char)
              , F <$> (arbitrary :: Gen Int)
              ]

instance Arbitrary (Const Int Char) where
    arbitrary = Const <$> (arbitrary :: Gen Char)

instance Arbitrary (Fail Int Char) where
    arbitrary = Fail <$> (arbitrary :: Gen Int)

instance Arbitrary Rand where
    arbitrary = Rand <$> (arbitrary :: Gen (Boolean Int Char))
