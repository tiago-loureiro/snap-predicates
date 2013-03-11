Snap-Predicates
===============

This library provides a definition of a type-class `Predicate`
together with several concrete implementations which are used to
constrain the set of possible `Snap` handlers in a type-safe
way.

Example
-------

```haskell
{-# LANGUAGE OverloadedStrings, TypeOperators #-}
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
        Accept "application/json"
        :&: (Param "name" :|: Param "nick")
        :&: Param "foo"

    get "/status" status $ Const 'x'

    post "/" createUser $
        Accept "application/x-thrift"

    post "/" createUser' $
        Accept "application/json"

getUser :: () :*: (ByteString :+: ByteString) :*: ByteString -> Snap ()
getUser (_ :*: name :*: foo) =
    case name of
        Left  a -> writeBS $ "getUser: name=" <> a <> " foo=" <> foo
        Right b -> writeBS $ "getUser: nick=" <> b <> " foo=" <> foo

createUser :: () -> Snap ()
createUser _ = writeBS "createUser"

createUser' :: () -> Snap ()
createUser' _ = writeBS "createUser'"

status :: Char -> Snap ()
status _ = writeBS "status"
```
