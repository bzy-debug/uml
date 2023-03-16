module Ast where

import Data.IORef (IORef)
import qualified Data.Text as T

type Name = T.Text

type Env a = [(Name, a)]

type Ref a = IORef a

data Def
  = DVal (Name, Expr)
  | DExpr Expr
  | DDefine (Name, Lambda)

data Expr
  = ELiteral Value
  | EVar Name
  | ESet (Name, Expr)
  | EIfx (Expr, Expr, Expr)
  | EWhilex (Expr, Expr)
  | EBegin [Expr]
  | EApply (Expr, [Expr])
  | ELetx (LetFlavor, [(Name, Expr)], Expr)
  | ELambda Lambda

data LetFlavor = Let | LetRec | LetStart

data Value
  = VSym Name
  | VNum Int
  | VBool Bool
  | VNil
  | VPair (Value, Value)
  | VClosure (Lambda, Env (Ref Value))
  | VPrimitve Primitive

type Lambda = ([Name], Expr)

type Primitive = [(Expr, Value)] -> Value

emptyEnv :: Env a
emptyEnv = []

instance Show Value where
  show (VSym v) = T.unpack v
  show (VNum n) = show n
  show (VBool b) = T.unpack $ if b then "#t" else "#f"
  show VNil = T.unpack "()"
  show (VPair (car, cdr)) =
    let tail' (VPair (car', cdr')) = " " ++ show car' ++ tail' cdr'
        tail' VNil = ")"
        tail' v = " . " ++ show v ++ ")"
     in "(" ++ show car ++ tail' cdr
  show (VClosure _) = "<function>"
  show (VPrimitve _) = "<function>"
