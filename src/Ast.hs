module Ast where

import Util
import qualified Data.Map as Map

type Name = String
type Location = Int

type Env = Map.Map Name Location

type Store = Map.Map Location Value

data Def
  = DVal (Name, Expr)
  | DExpr Expr
  | DDefine (Name, Lambda)

data Expr
  = ELiteral Value
  | EVar Name
  | ESet Name Expr
  | EIfx Expr Expr Expr
  | EWhilex Expr Expr
  | EBegin [Expr]
  | EApply Expr [Expr]
  | ELetx LetFlavor [(Name, Expr)] Expr
  | ELambda Lambda
  deriving Show

data LetFlavor = Let | LetRec | LetStar
  deriving Show

data Value
  = VSym Name
  | VNum Int
  | VBool Bool
  | VNil
  | VPair Value Value
  | VClosure Lambda Env
  | VPrimitve Primitive

type Lambda = ([Name], Expr)

type Primitive = Expr -> [Value] -> Either InterpException Value

emptyEnv :: Env
emptyEnv = Map.empty

instance Show Value where
  show (VSym v) = v
  show (VNum n) = show n
  show (VBool b) = if b then "#t" else "#f"
  show VNil = "()"
  show (VPair car cdr) =
    let tail' (VPair car' cdr') = " " ++ show car' ++ tail' cdr'
        tail' VNil = ")"
        tail' v = " . " ++ show v ++ ")"
     in "(" ++ show car ++ tail' cdr
  show (VClosure _ _) = "<function>"
  show (VPrimitve _) = "<function>"
