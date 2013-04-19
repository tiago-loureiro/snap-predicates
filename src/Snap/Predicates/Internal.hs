module Snap.Predicates.Internal
  ( headers
  , params
  , safeHead
  )
where

import Snap.Core hiding (headers)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import qualified Data.Map.Strict as M

headers :: Request -> ByteString -> [ByteString]
headers rq name = maybe [] id . getHeaders (mk name) $ rq

params :: Request -> ByteString -> [ByteString]
params rq name = maybe [] id . M.lookup name . rqParams $ rq

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (h:_) = Just h
