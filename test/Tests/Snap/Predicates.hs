{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}
module Tests.Snap.Predicates (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Predicate
import Snap.Predicates
import Snap.Predicates.MediaTypes
import Snap.Test
import qualified Data.Map.Strict as M

tests :: [Test]
tests =
    [ testAccept
    , testParam
    , testAcceptJson
    , testAcceptThrift
    ]

testAccept :: Test
testAccept = testCase "Accept Predicate" $ do
    rq0 <- buildRequest $ addHeader "Accept" "x/y"
    assertEqual "Matching Accept" (T ()) (apply (Accept "x/y") rq0)

    rq1 <- buildRequest $ get "/" M.empty
    assertEqual "Status Code 406"
        (F $ Just (406, Just "Expected 'Accept: x/y'."))
        (apply (Accept "x/y") rq1)

testAcceptJson :: Test
testAcceptJson = testCase "AcceptJson Predicate" $ do
    rq0 <- buildRequest $ addHeader "Accept" "application/json"
    assertEqual "Matching AcceptJson" (T AcceptJson) (apply AcceptJson rq0)

    rq1 <- buildRequest $ addHeader "Accept" "foo"
    assertEqual "Status Code 406"
        (F $ Just (406, Just "Expected 'Accept: application/json'."))
        (apply AcceptJson rq1)

testAcceptThrift :: Test
testAcceptThrift = testCase "AcceptThrift Predicate" $ do
    rq0 <- buildRequest $ addHeader "Accept" "application/x-thrift"
    assertEqual "Matching AcceptThrift" (T AcceptThrift) (apply AcceptThrift rq0)

    rq1 <- buildRequest $ addHeader "Accept" "application/json"
    assertEqual "Status Code 406"
        (F $ Just (406, Just "Expected 'Accept: application/x-thrift'."))
        (apply AcceptThrift rq1)

testParam :: Test
testParam = testCase "Param Predicate" $ do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    assertEqual "Matching Param" (T "y") (apply (Param "x") rq0)

    rq1 <- buildRequest $ get "/" M.empty
    assertEqual "Status Code 400"
        (F $ Just (400, Just "Expected parameter 'x'."))
        (apply (Param "x") rq1)
