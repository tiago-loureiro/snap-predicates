{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Predicate.Typeof where

import Data.Monoid
import Data.Predicate.Descr
import GHC.Generics

class Typeof a where
    typeof :: a -> Type

    default typeof :: (Generic a, GTypeof (Rep a)) => a -> Type
    typeof a = gtypeof (undefined `asTypeOf` (from a))

class GTypeof f where
    gtypeof :: f a -> Type

instance GTypeof U1 where
    gtypeof _ = TPrim PUnit

instance (Typeof a) => GTypeof (K1 R a) where
    gtypeof _ = typeof (undefined :: a)

instance (Datatype c, GTypeof f) => GTypeof (D1 c f) where
    gtypeof (m1 :: (D1 c f) a) =
        Type (datatypeName m1) (gtypeof (undefined :: f a))

instance (GTypeof f) => GTypeof (C1 c f) where
    gtypeof (_ :: (C1 c f) a) = gtypeof (undefined :: f a)

instance (GTypeof f, GTypeof g) => GTypeof (f :*: g) where
    gtypeof (_ :: (f :*: g) a) =
        let p1 = gtypeof (undefined :: f a)
            p2 = gtypeof (undefined :: g a)
        in case (p1, p2) of
            (TRec  x, TRec  y) -> TRec  (x <> y)
            (TProd x, TProd y) -> TProd (x <> y)
            _                  -> TProd [p1, p2]

instance (Selector s, GTypeof f) => GTypeof (S1 s f) where
    gtypeof (m1 :: (S1 s f) a) = case selName m1 of
        "" -> TProd [gtypeof (undefined :: f a)]
        nm -> TRec  [(nm, gtypeof (undefined :: f a))]

instance GTypeof (f :+: g) where
    gtypeof _ = TVoid

instance Typeof Int where typeof _ = TPrim PInt
instance Typeof Bool where typeof _ = TPrim PBool
instance Typeof Char where typeof _ = TPrim PChar
instance Typeof Float where typeof _ = TPrim PFloat
instance Typeof Double where typeof _ = TPrim PDouble

instance (Typeof a) => Typeof [a] where
    typeof _ = TSeq (typeof (undefined :: a))

instance (Typeof a) => Typeof (Maybe a) where
    typeof _ = Type "Maybe" (typeof (undefined :: a))

instance (Typeof a, Typeof b) => Typeof (Either a b) where
    typeof _ = Type "Either"
        (TSum [("Left", typeof (undefined :: a))
              ,("Right", typeof (undefined :: b))
              ])
