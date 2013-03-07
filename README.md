Example
-------

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import Data.Monoid
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
        Accept "application/json" :&: AnyParamOf ["name", "nick"]

    get "/status" status Anything

    get "/secret" secret (MyParamCheck "foo")

    post "/" createUser $
        Accept "application/x-thrift"

    post "/" createUser' $
        Accept "application/json"

getUser, createUser, createUser' :: MonadSnap m => m ()
getUser     = writeBS "getUser"
createUser  = writeBS "createUser"
createUser' = writeBS "createUser'"

status, secret :: MonadSnap m => m ()
status = writeBS "status"
secret = writeBS "secret"
```

`MyParamCheck` is a custom `Predicate` (and a somewhat contrived example):

```haskell
data MyParamCheck = MyParamCheck ByteString

instance Predicate MyParamCheck where
    apply (MyParamCheck x) r =
        maybe (Bad 400 (Just $ "Expecting: " <> x)) (const Good) canRead
      where
        canRead :: Maybe ()
        canRead = do
            ps <- rqParam x r
            if null . concat $ map readSingle ps
                then Nothing
                else Just ()

        readSingle :: ByteString -> [Int]
        readSingle p =
            case reads (CS.unpack p) of
                [(i, _)] -> [i]
                _        -> []

    toStr (MyParamCheck x) = "MyParamCheck: " <> x
```
