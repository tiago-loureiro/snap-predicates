{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tests.Snap.Predicate (tests) where

import Data.ByteString (ByteString)
import Data.Predicate
import Snap.Predicate
import Snap.Test
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as M

tests :: TestTree
tests = testGroup "Snap.Predicate"
    [ testCase "Accept Predicate" testAccept
    , testCase "AcceptJson Predicate" testParam
    , testCase "AcceptThrift Predicate" testAcceptThrift
    , testCase "Param Predicate" testAcceptJson
    , testCase "ParamOpt Predicate" testParamOpt
    ]

testAccept :: IO ()
testAccept = do
    rq0 <- buildRequest $ addHeader "Accept" "x/y"
    let predicate = Accept (Type "x") (SubType "y")
    let true = T 0 $ MediaType (Type "x") (SubType "y") 1.0 []
    assertEqual "Matching Accept" true (eval predicate rq0)

    rq1 <- buildRequest $ addHeader "Accept" "u/v"
    assertEqual "Status Code 406"
        (F (err 406 ("Expected 'Accept: \"x\"/\"y\"'.")))
        (eval predicate rq1)

testAcceptJson :: IO ()
testAcceptJson = do
    rq0 <- buildRequest $ addHeader "Accept" "application/json"
    let predicate = Accept Application Json
    let true = T 0 $ MediaType Application Json 1.0 []
    assertEqual "Matching AcceptJson" true (eval predicate rq0)

    rq1 <- buildRequest $ addHeader "Accept" "foo/bar"
    assertEqual "Status Code 406"
        (F (err 406 ("Expected 'Accept: application/json'.")))
        (eval predicate rq1)

testAcceptThrift :: IO ()
testAcceptThrift = do
    rq0 <- buildRequest $ addHeader "Accept" "application/x-thrift"
    let predicate = Accept Application Thrift
    let true = T 0 $ MediaType Application Thrift 1.0 []
    assertEqual "Matching AcceptThrift" true (eval predicate rq0)

    rq1 <- buildRequest $ addHeader "Accept" "application/json"
    assertEqual "Status Code 406"
        (F (err 406 ("Expected 'Accept: application/x-thrift'.")))
        (eval predicate rq1)

testParam :: IO ()
testParam = do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    assertEqual "Matching Param" (T 0 "y") (eval (Param "x" :: Param ByteString) rq0)

    rq1 <- buildRequest $ get "/" M.empty
    assertEqual "Status Code 400"
        (F (err 400 ("Missing parameter 'x'.")))
        (eval (Param "x" :: Param ByteString) rq1)

testParamOpt :: IO ()
testParamOpt = do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    assertEqual "Matching Param 1" (T 0 (Just "y")) (eval (ParamOpt "x" :: ParamOpt ByteString) rq0)

    rq1 <- buildRequest $ get "/" M.empty
    assertEqual "Matching Param 2" (T 0 Nothing) (eval (ParamOpt "x" :: ParamOpt ByteString) rq1)
