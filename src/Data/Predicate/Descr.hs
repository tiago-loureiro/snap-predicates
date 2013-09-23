module Data.Predicate.Descr where

type Name = String

data Type
    = TPrim  Prim
    | TProd  [Type]
    | TSum   [(Name, Type)]
    | TRec   [(Name, Type)]
    | TSeq   Type
    | TLabel Name
    | Type   Name Type
    | TVoid
    deriving Show

data Prim
    = PBool
    | PInt
    | PInteger
    | PFloat
    | PDouble
    | PChar
    | PUnit
    deriving (Eq, Show)

data Obligation
    = Required
    | Optional
    | Default !String
    deriving (Eq, Show)

data Descr
    = DConst  !Name   !String !Type       [Tag]
    | DSymbol !Name   !String !Obligation [Tag]
    | DValue  !Name   !Type   !Obligation [Tag]
    | DType   !Name   !Type               [Tag]
    | DDoc    !String
    | DLabel  !String !Descr
    | DEither !Descr  !Descr
    | DAll    !Descr  !Descr
    deriving Show

class Description a where
    describe :: a -> Descr

foldD :: (a -> Descr -> a) -> a -> Descr -> a
foldD f z (DAll   a b)  = foldD f (foldD f z a) b
foldD f z (DEither a b) = foldD f (foldD f z a) b
foldD f z d             = f z d

mapD :: (Descr -> a) -> Descr -> [a]
mapD f = foldD (\a d -> f d : a) []

filterD :: (Descr -> Bool) -> Descr -> [Descr]
filterD f = foldD (\a d -> if f d then d : a else a) []

name :: Descr -> Maybe Name
name (DConst  n _ _ _) = Just n
name (DSymbol n _ _ _) = Just n
name (DValue  n _ _ _) = Just n
name (DType   n _ _)   = Just n
name _                 = Nothing

obligation :: Descr -> Maybe Obligation
obligation (DSymbol _ _ o _) = Just o
obligation (DValue  _ _ o _) = Just o
obligation _                 = Nothing

typ :: Descr -> Maybe Type
typ (DConst  _ _ t _) = Just t
typ (DValue  _ t _ _) = Just t
typ (DType   _ t _)   = Just t
typ _                 = Nothing

typename :: Descr -> Maybe String
typename (DConst  _ _ (TLabel n) _) = Just n
typename (DValue  _ (TLabel n) _ _) = Just n
typename (DType n _ _)              = Just n
typename _                          = Nothing

tags :: Descr -> [Tag]
tags (DConst  _ _ _ t) = t
tags (DSymbol _ _ _ t) = t
tags (DValue  _ _ _ t) = t
tags (DType   _ _ t)   = t
tags _                 = []
