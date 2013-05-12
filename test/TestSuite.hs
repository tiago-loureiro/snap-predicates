module Main where

import Test.Framework (defaultMain, testGroup)
import qualified Tests.Data.Predicate as Predicate
import qualified Tests.Snap.Predicate as SnapPredicate
import qualified Tests.Snap.Route     as SnapRoute

main :: IO ()
main = defaultMain tests
  where
    tests =
        [ testGroup "Tests.Data.Predicate" Predicate.tests
        , testGroup "Tests.Snap.Predicate" SnapPredicate.tests
        , testGroup "Tests.Snap.Route"     SnapRoute.tests
        ]
