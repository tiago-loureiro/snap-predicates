module Data.Predicate.Descr where

type Name = String
type Tag  = String

data Type
    = TPrim  Prim
    | TProd  [Type]
    | TSum   [(Tag, Type)]
    | TRec   [(Name, Type)]
    | TSeq   Type
    | TLabel Name
    deriving Show

data Prim
    = PBool
    | PInt
    | PInteger
    | PFloat
    | PDouble
    | PChar
    | PUnit
    deriving Show

data Obligation
    = Required
    | Optional
    | Default !String
    deriving Show

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
