{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Tests.Snap.Route (tests) where

import Control.Applicative hiding (Const)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Predicate
import Data.String
import Snap.Core
import Snap.Predicate
import Snap.Predicate.Types
import Snap.Route
import Test.HUnit hiding (Test)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Snap.Test       as T
import qualified Data.Map.Strict as M
import qualified Data.Text       as Text

tests :: TestTree
tests = testGroup "Snap.Route"
    [ testCase "Sitemap" testSitemap
    , testCase "Media Selection" testMedia
    ]

testSitemap :: IO ()
testSitemap = do
    let routes  = expandRoutes sitemap
    let handler = route routes
    assertEqual "Endpoints" ["/a", "/b", "/c", "/d", "/e", "/f", "/z"] (map fst routes)
    testEndpointA handler
    testEndpointB handler
    testEndpointC handler
    testEndpointD handler
    testEndpointE handler
    testEndpointF handler

sitemap :: Routes Snap ()
sitemap = do
    get "/a" handlerA
        (Accept :&: (Param "name" :|: Param "nick") :&: Param "foo")

    get "/b" handlerB
        (Param "baz")

    get "/c" handlerC
        (ParamOpt "foo")

    get "/d" handlerD
        (ParamDef "foo" 0)

    get "/e" handlerE
        (HdrDef "foo" 0)

    get "/f" handlerF
        (Param "foo")

    get "/z" handlerZ
        (Fail (err 410 "Gone."))

handlerA :: Media "application" "json" :*: Int :*: ByteString -> Snap ()
handlerA (_ :*: i :*: _) = writeText (fromString . show $ i)

handlerB :: Int -> Snap ()
handlerB baz = writeText (Text.pack . show $ baz)

handlerC :: Maybe Int -> Snap ()
handlerC foo = writeText (Text.pack . show $ foo)

handlerD :: Int -> Snap ()
handlerD foo = writeText (Text.pack . show $ foo)

handlerE :: Int -> Snap ()
handlerE foo = writeText (Text.pack . show $ foo)

handlerF :: CSV Int -> Snap ()
handlerF foo = writeText (Text.pack . show . sum . list $ foo)

handlerZ :: Media "application" "json" -> Snap ()
handlerZ _ = do
    rq <- getRequest
    with (Param "bar" :&: Param "baz") rq $ \(bar :*: baz) -> do
        writeBS bar
        writeBS baz

testEndpointA :: Snap () -> Assertion
testEndpointA m = do
    let rq0 = T.get "/a" M.empty >> T.addHeader "Accept" "foo/bar"
    st0 <- rspStatus <$> T.runHandler rq0 m
    assertEqual "Accept fails" 406 st0

    let rq1 = rq0 >> T.addHeader "Accept" "application/json"
    st1 <- rspStatus <$> T.runHandler rq1 m
    assertEqual "Param fails" 400 st1

    let rq2 = T.get "/a" (M.fromList [("name", ["x"])]) >>
              T.addHeader "Accept" "application/json"
    st2 <- rspStatus <$> T.runHandler rq2 m
    assertEqual "Param fails" 400 st2

    let rq3 = T.get "/a" (M.fromList [("name", ["123"]), ("foo", ["\"y\""])]) >>
              T.addHeader "Accept" "application/json"
    T.runHandler rq3 m >>= T.assertSuccess

testEndpointB :: Snap () -> Assertion
testEndpointB m = do
    rs0 <- T.runHandler (T.get "/b" M.empty) m
    bd0 <- T.getResponseBody rs0
    assertEqual "b. baz 1" 400 (rspStatus rs0)
    assertEqual "b. baz 2" "Missing parameter 'baz'." bd0

    rs1 <- T.runHandler (T.get "/b" $ M.fromList [("baz", ["abc"])]) m
    bd1 <- T.getResponseBody rs1
    assertEqual "b. baz 3" 400 (rspStatus rs1)
    assertEqual "b. baz 4" "no read" bd1

    rs2 <- T.runHandler (T.get "/b" $ M.fromList [("baz", ["abc", "123"])]) m
    bd2 <- T.getResponseBody rs2
    assertEqual "b. baz 5" 200 (rspStatus rs2)
    assertEqual "b. baz 6" "123" bd2

testEndpointC :: Snap () -> Assertion
testEndpointC m = do
    rs0 <- T.runHandler (T.get "/c" M.empty) m
    bd0 <- T.getResponseBody rs0
    assertEqual "c. foo 1" 200 (rspStatus rs0)
    assertEqual "c. foo 2" "Nothing" bd0

    rs1 <- T.runHandler (T.get "/c" $ M.fromList [("foo", ["abc", "123"])]) m
    bd1 <- T.getResponseBody rs1
    assertEqual "c. foo 3" 200 (rspStatus rs1)
    assertEqual "c. foo 4" "Just 123" bd1

    rs2 <- T.runHandler (T.get "/c" $ M.fromList [("foo", ["abc"])]) m
    bd2 <- T.getResponseBody rs2
    assertEqual "c. foo 5" 400 (rspStatus rs2)
    assertEqual "c. foo 6" "no read" bd2

testEndpointD :: Snap () -> Assertion
testEndpointD m = do
    rs0 <- T.runHandler (T.get "/d" M.empty) m
    bd0 <- T.getResponseBody rs0
    assertEqual "d. foo 1" 200 (rspStatus rs0)
    assertEqual "d. foo 2" "0" bd0

    rs1 <- T.runHandler (T.get "/d" $ M.fromList [("foo", ["xxx", "42"])]) m
    bd1 <- T.getResponseBody rs1
    assertEqual "d. foo 3" 200 (rspStatus rs1)
    assertEqual "d. foo 4" "42" bd1

    rs2 <- T.runHandler (T.get "/d" $ M.fromList [("foo", ["yyy"])]) m
    bd2 <- T.getResponseBody rs2
    assertEqual "d. foo 5" 400 (rspStatus rs2)
    assertEqual "d. foo 6" "no read" bd2

testEndpointE :: Snap () -> Assertion
testEndpointE m = do
    rs0 <- T.runHandler (T.get "/e" M.empty) m
    bd0 <- T.getResponseBody rs0
    assertEqual "e. foo 1" 200 (rspStatus rs0)
    assertEqual "e. foo 2" "0" bd0

    rs1 <- T.runHandler (T.get "/e" M.empty >> T.addHeader "foo" "42") m
    bd1 <- T.getResponseBody rs1
    assertEqual "e. foo 3" 200 (rspStatus rs1)
    assertEqual "e. foo 4" "42" bd1

    rs2 <- T.runHandler (T.get "/e" M.empty >> T.addHeader "foo" "abc") m
    bd2 <- T.getResponseBody rs2
    assertEqual "e. foo 5" 400 (rspStatus rs2)
    assertEqual "e. foo 6" "no read" bd2

testEndpointF :: Snap () -> Assertion
testEndpointF m = do
    rs0 <- T.runHandler (T.get "/f" $ M.fromList [("foo", ["1,2,3,4"])]) m
    bd0 <- T.getResponseBody rs0
    assertEqual "e. foo 1" 200 (rspStatus rs0)
    assertEqual "e. foo 2" "10" bd0

-- Media Selection Tests:

testMedia :: IO ()
testMedia = do
    let [(_, h)] = expandRoutes sitemapMedia
    expectMedia "application/json;q=0.3, application/x-thrift;q=0.7" "application/x-thrift" h
    expectMedia "application/json;q=0.7, application/x-thrift;q=0.3" "application/json" h

sitemapMedia :: Routes Snap ()
sitemapMedia = do
    get "/media" handlerJson   $ Accept
    get "/media" handlerThrift $ Accept

handlerJson :: Media "application" "json" -> Snap ()
handlerJson _ = writeBS "application/json"

handlerThrift :: Media "application" "x-thrift" -> Snap ()
handlerThrift _ = writeBS "application/x-thrift"

expectMedia :: ByteString -> ByteString -> Snap () -> Assertion
expectMedia hdr res m = do
    let rq0 = T.get "/media" M.empty >>
              T.addHeader "Accept" hdr
    txt0 <- T.runHandler rq0 m >>= liftIO . T.getResponseBody
    assertEqual "media type" res txt0
