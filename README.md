Example
-------

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import Data.Monoid
import Predicates
import Snap.Core
import Snap.Routes
import Snap.Predicates
import Snap.Http.Server
import qualified Data.ByteString.Char8 as CS

main :: IO ()
main = do
    mapM_ CS.putStrLn (showRoutes sitemap)
    quickHttpServe (route . expandRoutes $ sitemap)

sitemap :: Routes ()
sitemap = do
    get "/" getUser $
        Accept "application/json" :&: Param "name"

    get "/status" status $ Const 'x'

    post "/" createUser $
        Accept "application/x-thrift"

    post "/" createUser' $
        Accept "application/json"

getUser :: ((), ByteString) -> Snap ()
getUser (_, v) = writeBS $ "getUser: " <> v

createUser :: () -> Snap ()
createUser _ = writeBS "createUser"

createUser' :: () -> Snap ()
createUser' _ = writeBS "createUser'"

status :: Char -> Snap ()
status _ = writeBS "status"
