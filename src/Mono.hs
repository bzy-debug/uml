module Mono where

type Name = String

type TyVar = Name
type TyCon = Name

type Env a = [(Name, a)]

data Typ
  = TVar TyVar
  | TCon TyCon
  | ConApp Typ [Typ]

data TypScheme = Forall [Name] Typ
