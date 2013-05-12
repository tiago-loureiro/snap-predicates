{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Snap.Predicates.MediaTypes.Internal where

import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.List (sortBy)
import Data.Maybe
import Snap.Core (Request)
import Snap.Predicates.Internal
import Snap.Predicates.MediaTypes
import qualified Data.Predicate.Env as E
import qualified Snap.Predicates.Parsers.Accept as A

mediaType :: (MType t, MSubType s) => Bool -> t -> s -> [A.MediaType] -> Maybe (MediaType t s)
mediaType fuzzy t s = safeHead . mapMaybe (\m -> do
    t' <- if fuzzy && A.medType    m == "*" then Just t else toType t    (A.medType m)
    s' <- if fuzzy && A.medSubtype m == "*" then Just s else toSubType s (A.medSubtype m)
    guard (t == t' && s == s')
    return $ MediaType t s (A.medQuality m) (A.medParams m))

readMediaTypes :: (MonadState m, StateType m ~ E.Env) => ByteString -> Request -> m [A.MediaType]
readMediaTypes k r = do
    let mtypes = sortBy q . concat . map A.parseMediaTypes $ headers k r
    E.insert k mtypes
    return mtypes
  where
    q a b = A.medQuality b `compare` A.medQuality a
