{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}
module Tests.Snap.Predicates (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Monad.State.Strict hiding (get)
import Data.Predicate
import Snap.Predicates
import Snap.Predicates.Params
import Snap.Predicates.MediaTypes
import Snap.Test
import qualified Data.Map.Strict as M
import qualified Data.Predicate.Env as E

tests :: [Test]
tests =
    [ testAccept
    , testParam
    , testAcceptJson
    , testAcceptThrift
    ]

eval :: Predicate p a => p -> a -> Boolean (FVal p) (TVal p)
eval p r = evalState (apply p r) E.empty

testAccept :: Test
testAccept = testCase "Accept Predicate" $ do
    rq0 <- buildRequest $ addHeader "Accept" "x/y"
    let predicate = Accept (Typ "x") (SubTyp "y")
    let true = T $ MediaType (Typ "x") (SubTyp "y") 1.0 []
    assertEqual "Matching Accept" true (eval predicate rq0)

    rq1 <- buildRequest $ get "/" M.empty
    assertEqual "Status Code 406"
        (F (err 406 ("Expected 'Accept: \"x\"/\"y\"'.")))
        (eval predicate rq1)

testAcceptJson :: Test
testAcceptJson = testCase "AcceptJson Predicate" $ do
    rq0 <- buildRequest $ addHeader "Accept" "application/json"
    let predicate = Accept Application Json
    let true = T $ MediaType Application Json 1.0 []
    assertEqual "Matching AcceptJson" true (eval predicate rq0)

    rq1 <- buildRequest $ addHeader "Accept" "foo"
    assertEqual "Status Code 406"
        (F (err 406 ("Expected 'Accept: application/json'.")))
        (eval predicate rq1)

testAcceptThrift :: Test
testAcceptThrift = testCase "AcceptThrift Predicate" $ do
    rq0 <- buildRequest $ addHeader "Accept" "application/x-thrift"
    let predicate = Accept Application Thrift
    let true = T $ MediaType Application Thrift 1.0 []
    assertEqual "Matching AcceptThrift" true (eval predicate rq0)

    rq1 <- buildRequest $ addHeader "Accept" "application/json"
    assertEqual "Status Code 406"
        (F (err 406 ("Expected 'Accept: application/x-thrift'.")))
        (eval predicate rq1)

testParam :: Test
testParam = testCase "Param Predicate" $ do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    assertEqual "Matching Param" (T "y") (eval (Param "x") rq0)

    rq1 <- buildRequest $ get "/" M.empty
    assertEqual "Status Code 400"
        (F (err 400 ("Expected parameter 'x'.")))
        (eval (Param "x") rq1)
