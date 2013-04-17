{-# LANGUAGE TemplateHaskell #-}
module Snap.Predicates.Internal
  ( headers
  , params
  , safeHead
  , defineType
  , defineSubType
  )
where

import Snap.Core hiding (headers)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Language.Haskell.TH
import qualified Data.Map.Strict as M

headers :: Request -> ByteString -> [ByteString]
headers rq name = maybe [] id . getHeaders (mk name) $ rq

params :: Request -> ByteString -> [ByteString]
params rq name = maybe [] id . M.lookup name . rqParams $ rq

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (h:_) = Just h


defineType :: String -> String -> Q [Dec]
defineType name repr =
    return [ dataDecl name
           , instDecl "MType" "toType" name repr
           , showDecl name repr
           ]

defineSubType :: String -> String -> Q [Dec]
defineSubType name repr =
    return [ dataDecl name
           , instDecl "MSubType" "toSubType" name repr
           , showDecl name repr
           ]

dataDecl :: String -> Dec
dataDecl name = DataD [] (mkName name) [] [NormalC (mkName name) []] [mkName "Eq"]

instDecl :: String -> String -> String -> String -> Dec
instDecl clazz fname name repr =
    let name' = mkName name in
    InstanceD [] (AppT (ConT (mkName clazz)) (ConT name'))
        [ FunD (mkName fname)
            [ Clause [WildP, VarP (mkName "s")]
                (NormalB
                    (CaseE (VarE (mkName "s"))
                        [ Match (LitP (StringL repr)) (NormalB (AppE (ConE (mkName "Just")) (ConE name'))) []
                        , Match WildP (NormalB (ConE (mkName "Nothing"))) []
                        ]))
                []
            ]
        ]
showDecl :: String -> String -> Dec
showDecl name repr =
    let name' = mkName name in
    InstanceD [] (AppT (ConT (mkName "Show")) (ConT name'))
        [ FunD (mkName "show") [ Clause [WildP] (NormalB (LitE (StringL repr))) []]]
