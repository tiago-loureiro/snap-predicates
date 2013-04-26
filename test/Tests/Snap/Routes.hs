{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Tests.Snap.Routes (tests) where

import Control.Applicative hiding (Const)
import Control.Monad.IO.Class
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.ByteString (ByteString)
import Data.Either
import Data.Predicate
import Data.String
import Data.Text (Text, strip)
import Data.Text.Encoding
import Snap.Core
import Snap.Predicates hiding (Text)
import Snap.Routes
import qualified Snap.Test as T
import qualified Data.Map.Strict as M
import qualified Data.Text as Text

tests :: [Test]
tests =
    [ testSitemap
    , testMedia
    ]

testSitemap :: Test
testSitemap = testCase "Sitemap" $ do
    let routes = expandRoutes sitemap
    assertEqual "Endpoints" ["/a", "/b", "/c", "/d", "/e"] (map fst routes)
    mapM_ (\(r, h) -> h r) (zip (map snd routes) [testEndpointA])

sitemap :: Routes Snap ()
sitemap = do
    get "/a" handlerA $
        Accept Application Json :&: (Param "name" :|: Param "nick") :&: Param "foo"

    get "/b" handlerB $
        Accept Application Json :&: (Param "name" :||: Param "nick") :&: Param "foo"

    get "/c" handlerC $ Fail (err 410 "Gone.")

    post "/d" handlerD $ Accept Application Json :&: Parameter "foo" decode Nothing

    get "/e" handlerE $ (Param "foo" :|: Const 0) :&: ParamOpt "bar"
  where
    decode bs =
        let txt = rights (map decodeUtf8' bs)
        in if null txt
               then Left "UTF-8 decoding error"
               else Right (map strip txt)

handlerA :: MediaType Application Json :*: Int :*: ByteString -> Snap ()
handlerA (_ :*: i :*: _) = writeText (fromString . show $ i)

handlerB :: MediaType Application Json :*: (ByteString :+: ByteString) :*: ByteString -> Snap ()
handlerB (_ :*: name :*: _) =
    case name of
        Left  _ -> return ()
        Right _ -> return ()

handlerC :: MediaType Application Json -> Snap ()
handlerC _ = do
    rq <- getRequest
    with (Param "bar" :&: Param "baz") rq $ \(bar :*: baz) -> do
        writeBS bar
        writeBS baz

handlerD :: MediaType Application Json :*: [Text] -> Snap ()
handlerD (_ :*: txt) = writeText $ Text.intercalate ", " txt

handlerE :: Int :*: Maybe ByteString -> Snap ()
handlerE (foo :*: Just bar) = writeText (Text.pack . show $ foo) >> writeBS bar
handlerE (_   :*: Nothing)  = return ()

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

    let rq3 = T.get "/a" (M.fromList [("name", ["123"]), ("foo", ["y"])]) >>
              T.addHeader "Accept" "application/json"
    T.runHandler rq3 m >>= T.assertSuccess

-- Media Selection Tests:

testMedia :: Test
testMedia = testCase "Media Selection" $ do
    let [(_, h)] = expandRoutes sitemapMedia
    expectMedia "application/json;q=0.3, application/x-thrift;q=0.7" "application/x-thrift" h
    expectMedia "application/json;q=0.7, application/x-thrift;q=0.3" "application/json" h

sitemapMedia :: Routes Snap ()
sitemapMedia = do
    get "/media" handlerJson   $ Accept Application Json
    get "/media" handlerThrift $ Accept Application Thrift

handlerJson :: MediaType Application Json -> Snap ()
handlerJson _ = writeBS "application/json"

handlerThrift :: MediaType Application Thrift -> Snap ()
handlerThrift _ = writeBS "application/x-thrift"

expectMedia :: ByteString -> ByteString -> Snap () -> Assertion
expectMedia hdr res m = do
    let rq0 = T.get "/media" M.empty >>
              T.addHeader "Accept" hdr
    txt0 <- T.runHandler rq0 m >>= liftIO . T.getResponseBody
    assertEqual "media type" res txt0
