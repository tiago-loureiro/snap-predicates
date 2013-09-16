module Data.Predicate.Descr where

import Data.Typeable

type Name = String

data Value
    = Sym !String
    | Val !String !TypeRep
    | Def !String !TypeRep
    | Typ !TypeRep
    deriving Show

data Obligation
    = Required
    | Optional
    deriving Show

data Descr
    = DConst  !Name  !Value
    | DParam  !Name  !Value !Obligation
    | DHeader !Name  !Value !Obligation
    | DEither !Descr !Descr
    | DAll    !Descr !Descr
    deriving Show

class Description a where
    describe :: a -> Descr
