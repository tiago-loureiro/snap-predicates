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
    [ testCase "Accept application/json" testParam
    , testCase "Accept application/thrift " testAcceptThrift
    , testCase "Accept application/*" testAcceptAll
    , testCase "Content-Type text/plain" testContentTypePlain
    , testCase "Content-Type text/*" testContentTypeAll
    , testCase "Param" testAcceptJson
    , testCase "ParamOpt" testParamOpt
    ]

testAcceptJson :: IO ()
testAcceptJson = do
    rq0 <- buildRequest $ addHeader "Accept" "application/json"
    (T 0 $ MediaType Application Json 1.0 []) @=? (eval (Accept Application Json) rq0)

    rq1 <- buildRequest $ addHeader "Accept" "foo/bar"
    (F (err 406 ("Expected 'Accept: application/json'."))) @=? (eval (Accept Application Json) rq1)

testAcceptThrift :: IO ()
testAcceptThrift = do
    rq0 <- buildRequest $ addHeader "Accept" "application/x-thrift"
    (T 0 $ MediaType Application Thrift 1.0 []) @=? (eval (Accept Application Thrift) rq0)

    rq1 <- buildRequest $ addHeader "Accept" "application/json"
    (F (err 406 ("Expected 'Accept: application/x-thrift'."))) @=? (eval (Accept Application Thrift) rq1)

testAcceptAll :: IO ()
testAcceptAll = do
    rq0 <- buildRequest $ addHeader "Accept" "application/*"
    (T 0 $ MediaType Application All 1.0 []) @=? eval (Accept Application All) rq0

    rq1 <- buildRequest $ addHeader "Accept" "application/*"
    (T 0 $ MediaType Application Json 1.0 []) @=? eval (Accept Application Json) rq1

testContentTypePlain :: IO ()
testContentTypePlain = do
    rq0 <- buildRequest $ postRaw "/" "text/plain" "hello"
    (T 0 $ MediaType Text Plain 1.0 []) @=? (eval (ContentType Text Plain) rq0)

    rq1 <- buildRequest $ postRaw "/" "text/html" "hello"
    (F (err 415 ("Expected 'Content-Type: text/plain'."))) @=? (eval (ContentType Text Plain) rq1)

testContentTypeAll :: IO ()
testContentTypeAll = do
    rq0 <- buildRequest $ postRaw "/" "text/plain" "hello"
    (T 0.5 $ MediaType Text All 0.5 []) @=? (eval (ContentType Text All) rq0)

testParam :: IO ()
testParam = do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    (T 0 "y") @=? (eval (Param "x" :: Param ByteString) rq0)

    rq1 <- buildRequest $ get "/" M.empty
    (F (err 400 ("Missing parameter 'x'."))) @=? (eval (Param "x" :: Param ByteString) rq1)

testParamOpt :: IO ()
testParamOpt = do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    (T 0 (Just "y")) @=? (eval (ParamOpt "x" :: ParamOpt ByteString) rq0)

    rq1 <- buildRequest $ get "/" M.empty
    (T 0 Nothing) @=? (eval (ParamOpt "x" :: ParamOpt ByteString) rq1)
