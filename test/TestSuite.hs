module Main where

import Test.Tasty
import qualified Tests.Data.Predicate as Predicate
import qualified Tests.Snap.Predicate as SnapPredicate
import qualified Tests.Snap.Route     as SnapRoute

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Predicate.tests
    , SnapPredicate.tests
    , SnapRoute.tests
    ]
