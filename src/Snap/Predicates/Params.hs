{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Predicates.Params
  ( Parameter (..)
  , Param (..)
  , ParamOpt (..)
  )
where

import Data.ByteString (ByteString)
import Data.Either
import Data.Monoid
import Data.Typeable
import Data.Predicate
import Data.Predicate.Readable
import Snap.Core hiding (headers)
import Snap.Predicates
import Snap.Predicates.Internal
import qualified Data.Predicate.Env as E
import qualified Data.ByteString as S

-- | The most generic request parameter predicate provided.
-- It will get all request parameter values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. In case of error, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data Parameter a = Parameter
  { _name    :: !ByteString                         -- ^ request parameter name
  , _read    :: [ByteString] -> Either ByteString a -- ^ conversion function
  , _default :: !(Maybe a)                          -- ^ (optional) default value
  }

instance Typeable a => Predicate (Parameter a) Request where
    type FVal (Parameter a) = Error
    type TVal (Parameter a) = a
    apply (Parameter nme f def) r =
        E.lookup (key nme) >>= maybe work result
      where
        work = case params r nme of
            [] -> maybe (return (F (err 400 ("Missing parameter '" <> nme <> "'."))))
                        (return . (T 0))
                        def
            vs -> do
                let x = f vs
                E.insert (key nme) x
                case x of
                    Left msg -> return $ maybe (F (err 400 msg)) (T 0) def
                    Right  v -> return $ T 0 v

        result (Left msg) = return (F (err 400 msg))
        result (Right  v) = return (T 0 v)

        key = ("parameter:" <>)

instance Show (Parameter a) where
    show p = "Parameter: " ++ show (_name p)

-- | Specialisation of 'Parameter' which returns the first request
-- which could be converted to the target type.
data Param a = Param ByteString

instance (Typeable a, Readable a) => Predicate (Param a) Request where
    type FVal (Param a) = Error
    type TVal (Param a) = a
    apply (Param x)     = apply (Parameter x f Nothing)
      where
        f vs = let (es, xs) = partitionEithers $ map fromByteString vs
               in if null xs
                      then Left (S.intercalate "\n" es)
                      else Right (head xs)

instance Show (Param a) where
    show (Param x) = "Param: " ++ show x


data ParamOpt a = ParamOpt ByteString

instance (Typeable a, Readable a) => Predicate (ParamOpt a) Request where
    type FVal (ParamOpt a) = Error
    type TVal (ParamOpt a) = Maybe a
    apply (ParamOpt x)     = apply (Parameter x f (Just Nothing))
      where
        f vs = let xs = rights $ map fromByteString vs
               in if null xs then Right Nothing else Right (head xs)

instance Show (ParamOpt a) where
    show (ParamOpt x) = "ParamOpt: " ++ show x
