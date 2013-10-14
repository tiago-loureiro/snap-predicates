{-# LANGUAGE DataKinds         #-}
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
    (T 0 $ Media "application" "json" 1.0 []) @=? (apply (accept :: Accept "application" "json") rq0)

    rq1 <- buildRequest $ addHeader "Accept" "foo/bar"
    (F (err 406 ("Expected 'Accept: application/json'."))) @=? (apply (accept :: Accept "application" "json") rq1)

testAcceptThrift :: IO ()
testAcceptThrift = do
    rq0 <- buildRequest $ addHeader "Accept" "application/x-thrift"
    (T 0 $ Media "application" "x-thrift" 1.0 []) @=? (apply (accept :: Accept "application" "x-thrift") rq0)

    rq1 <- buildRequest $ addHeader "Accept" "application/json"
    (F (err 406 ("Expected 'Accept: application/x-thrift'."))) @=? (apply (accept :: Accept "application" "x-thrift") rq1)

testAcceptAll :: IO ()
testAcceptAll = do
    rq0 <- buildRequest $ addHeader "Accept" "application/*"
    (T 0 $ Media "application" "*" 1.0 []) @=? apply (accept :: Accept "application" "*") rq0

    rq1 <- buildRequest $ addHeader "Accept" "application/*"
    (T 0 $ Media "application" "json" 1.0 []) @=? apply (accept :: Accept "application" "json") rq1

testContentTypePlain :: IO ()
testContentTypePlain = do
    rq0 <- buildRequest $ postRaw "/" "text/plain" "hello"
    (T 0 $ Media "text" "plain" 1.0 []) @=? (apply (contentType :: ContentType "text" "plain") rq0)

    rq1 <- buildRequest $ postRaw "/" "text/html" "hello"
    (F (err 415 ("Expected 'Content-Type: text/plain'."))) @=? (apply (contentType :: ContentType "text" "plain") rq1)

testContentTypeAll :: IO ()
testContentTypeAll = do
    rq0 <- buildRequest $ postRaw "/" "text/plain" "hello"
    (T 0.5 $ Media "text" "plain" 0.5 []) @=? (apply (contentType :: ContentType "text" "*") rq0)

testParam :: IO ()
testParam = do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    (T 0 "y") @=? (apply (param "x" :: Param ByteString) rq0)

    rq1 <- buildRequest $ get "/" M.empty
    (F (err 400 ("Missing parameter 'x'."))) @=? (apply (param "x" :: Param ByteString) rq1)

testParamOpt :: IO ()
testParamOpt = do
    rq0 <- buildRequest $ get "/" (M.fromList [("x", ["y", "z"])])
    (T 0 (Just "y")) @=? (apply (paramOpt "x" :: ParamOpt ByteString) rq0)

    rq1 <- buildRequest $ get "/" M.empty
    (T 0 Nothing) @=? (apply (paramOpt "x" :: ParamOpt ByteString) rq1)
