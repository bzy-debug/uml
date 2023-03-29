{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ast where

import Control.Monad.State

type Name = String

type TyVar = Name

type TyCon = Name

type Env a = [(Name, a)]

type Ref = Int

data RefState = RefState
  { mem :: [(Ref, Env Value)],
    ref :: Ref
  }

newRef :: Env Value -> EvalMonad Ref
newRef env = do
  RefState {mem = mem, ref = ref} <- get
  put $
    RefState
      { mem = (ref, env) : mem,
        ref = ref + 1
      }
  return $ ref + 1

type EvalMonad = StateT RefState (Either String)

data Exp
  = Literal Value
  | Var Name
  | If Exp Exp Exp
  | Begin [Exp]
  | Apply Exp [Exp]
  | Letx LetFlavor [(Name, Exp)] Exp
  | Lambda [Name] Exp

data LetFlavor = Let | LetRec | LetStar
  deriving (Show)

data Value
  = Sym Name
  | Num Int
  | Bool Bool
  | Nil
  | Pair Value Value
  | Closure [Name] Exp Ref
  | Primitive Primitive

type Primitive = Exp -> [Value] -> EvalMonad Value

instance Show Value where
  show (Sym v) = v
  show (Num n) = show n
  show (Bool b) = if b then "#t" else "#f"
  show Nil = "()"
  show (Pair car cdr) =
    let tail' (Pair car' cdr') = " " ++ show car' ++ tail' cdr'
        tail' Nil = ")"
        tail' v = " . " ++ show v ++ ")"
     in "(" ++ show car ++ tail' cdr
  show (Closure {}) = "<closure>"
  show (Primitive _) = "<primitive>"
