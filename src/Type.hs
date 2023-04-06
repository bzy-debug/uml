module Type where

import Ast

data Type
  = TVar Name
  | TCon Name
  | TApp Name [Type]
  deriving (Eq, Show)
