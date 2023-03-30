{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ast where

import Control.Monad.State

type Name = String

type Env a = [(Name, a)]

type Ref = Int

data RefState = RefState
  { mem :: [(Ref, Env Value)],
    ref :: Ref
  }

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

instance Show Exp where
  show e =
    case e of
      Literal v -> show v
      Var x -> x
      If e1 e2 e3 -> parenSpace $ "if" : exps [e1, e2, e3]
      Begin es -> parenSpace $ "begin" : exps es
      Apply e es -> parenSpace $ exps (e : es)
      Letx lk bs e -> parenSpace [show lk, bindings bs, show e]
      Lambda xs body -> parenSpace ["lambda", parenSpace xs, show body]
    where
      paren s = "(" ++ s ++ ")"
      bracket s = "[" ++ s ++ "]"
      parenSpace = paren . unwords
      exps = map show
      binding (x, e) = bracket $ x ++ " " ++ show e
      bindings bs = (paren . unwords) (map binding bs)

instance Show LetFlavor where
  show Let = "let"
  show LetRec = "letrec"
  show LetStar = "let*"

data Value
  = Sym Name
  | Num Int
  | Bool Bool
  | Nil
  | Pair Value Value
  | Closure ([Name], Exp) Ref
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
