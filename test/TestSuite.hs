module Main where

import Test.Framework (defaultMain, testGroup)
import qualified Tests.Data.Predicate as Predicate
import qualified Tests.Snap.Predicates as SnapPredicates
import qualified Tests.Snap.Routes as SnapRoutes

main :: IO ()
main = defaultMain tests
  where
    tests =
        [ testGroup "Tests.Data.Predicate"  Predicate.tests
        , testGroup "Tests.Snap.Predicates" SnapPredicates.tests
        , testGroup "Tests.Snap.Routes"     SnapRoutes.tests
        ]
