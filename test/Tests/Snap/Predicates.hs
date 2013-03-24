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
    let predicate = Accept (Typ "x") (SubTyp "y")
    let true = T $ MediaType (Typ "x") (SubTyp "y") "1.0" []
    assertEqual "Matching Accept" true (apply predicate rq0)

    rq1 <- buildRequest $ get "/" M.empty
    assertEqual "Status Code 406"
        (F $ Just (406, Just "Expected 'Accept: \"x\"/\"y\"'."))
        (apply predicate rq1)

testAcceptJson :: Test
testAcceptJson = testCase "AcceptJson Predicate" $ do
    rq0 <- buildRequest $ addHeader "Accept" "application/json"
    let predicate = Accept Application Json
    let true = T $ MediaType Application Json "1.0" []
    assertEqual "Matching AcceptJson" true (apply predicate rq0)

    rq1 <- buildRequest $ addHeader "Accept" "foo"
    assertEqual "Status Code 406"
        (F $ Just (406, Just "Expected 'Accept: application/json'."))
        (apply predicate rq1)

testAcceptThrift :: Test
testAcceptThrift = testCase "AcceptThrift Predicate" $ do
    rq0 <- buildRequest $ addHeader "Accept" "application/x-thrift"
    let predicate = Accept Application Thrift
    let true = T $ MediaType Application Thrift "1.0" []
    assertEqual "Matching AcceptThrift" true (apply predicate rq0)

    rq1 <- buildRequest $ addHeader "Accept" "application/json"
    assertEqual "Status Code 406"
        (F $ Just (406, Just "Expected 'Accept: application/x-thrift'."))
        (apply predicate rq1)

testParam :: Test
testParam = testCase "Param Predicate" $ do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    assertEqual "Matching Param" (T "y") (apply (Param "x") rq0)

    rq1 <- buildRequest $ get "/" M.empty
    assertEqual "Status Code 400"
        (F $ Just (400, Just "Expected parameter 'x'."))
        (apply (Param "x") rq1)
