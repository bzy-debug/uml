module Constraint where

import Ast
import Type

data Constraint
  = Ceq Type Type
  | Cand Constraint Constraint
  | Trival

instance Show Constraint where
  show = show' . untriviate
    where
      show' (Ceq t t') = show t ++ " ~ " ++ show t'
      show' (Cand c c') = show c ++ " /\\ " ++ show c'
      show' Trival = "T"

untriviate :: Constraint -> Constraint
untriviate (Cand c c') =
  case (untriviate c, untriviate c') of
    (Trival, c) -> c
    (c, Trival) -> c
    (c, c') -> Cand c c'
untriviate atomic = atomic
