Snap-Predicates
===============

This library provides a definition of a type-class `Predicate`
together with several concrete implementations which are used to
constrain the set of possible `Snap` handlers in a type-safe
way.

Example
-------

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Predicate
import Snap.Core
import Snap.Routes
import Snap.Predicates
import Snap.Http.Server
import qualified Data.ByteString.Char8 as CS

main :: IO ()
main = do
    mapM_ CS.putStrLn (showRoutes sitemap)
    quickHttpServe (route . expandRoutes $ sitemap)

sitemap :: Routes Snap ()
sitemap = do
    get "/" getUser $
        Accept "application/json" :&: Param "name"

    get "/status" status $ Fail (400, Just "/status closed")

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
```
