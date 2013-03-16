{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Tests.Snap.Routes (tests) where

import Control.Applicative hiding (Const)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.ByteString (ByteString)
import Data.Predicate
import Data.Text (Text, strip)
import Data.Text.Encoding
import Snap.Core
import Snap.Predicates
import Snap.Routes
import qualified Snap.Test as T
import qualified Data.Map.Strict as M
import qualified Data.Text as Text

tests :: [Test]
tests =
    [ testSitemap ]

testSitemap :: Test
testSitemap = testCase "Sitemap" $ do
    let routes = expandRoutes sitemap
    assertEqual "Endpoints" ["/a", "/b", "/c", "/d"] (map fst routes)
    mapM_ (\(r, h) -> h r) (zip (map snd routes) [testEndpointA])

sitemap :: Routes Snap ()
sitemap = do
    get "/a" handlerA $
        AcceptJson :&: (Param "name" :|: Param "nick") :&: Param "foo"

    get "/b" handlerB $
        AcceptJson :&: (Param "name" :||: Param "nick") :&: Param "foo"

    get  "/c" handlerC $ Fail (410, Just "Gone.")

    post "/d" handlerD $ AcceptThrift :&: ParamTrans "foo" (map (strip . decodeUtf8))

handlerA :: AcceptJson :*: ByteString :*: ByteString -> Snap ()
handlerA (_ :*: _ :*: _) = return ()

handlerB :: AcceptJson :*: (ByteString :+: ByteString) :*: ByteString -> Snap ()
handlerB (_ :*: name :*: _) =
    case name of
        Left  _ -> return ()
        Right _ -> return ()

handlerC :: AcceptThrift -> Snap ()
handlerC _ = do
    rq <- getRequest
    with (Param "bar" :&: Param "baz") rq $ \(bar :*: _) ->
        writeBS bar

handlerD :: AcceptThrift :*: [Text] -> Snap ()
handlerD (_ :*: txt) = writeText $ Text.intercalate ", " txt

testEndpointA :: Snap () -> Assertion
testEndpointA m = do
    let rq0 = T.get "/a" M.empty
    st0 <- rspStatus <$> T.runHandler rq0 m
    assertEqual "Accept fails" 406 st0

    let rq1 = rq0 >> T.addHeader "Accept" "application/json"
    st1 <- rspStatus <$> T.runHandler rq1 m
    assertEqual "Param fails" 400 st1

    let rq2 = T.get "/a" (M.fromList [("name", ["x"])]) >>
              T.addHeader "Accept" "application/json"
    st2 <- rspStatus <$> T.runHandler rq2 m
    assertEqual "Param fails" 400 st2

    let rq3 = T.get "/a" (M.fromList [("name", ["x"]), ("foo", ["y"])]) >>
              T.addHeader "Accept" "application/json"
    T.runHandler rq3 m >>= T.assertSuccess

