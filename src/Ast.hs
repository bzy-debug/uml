module Ast where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

type Name = String

type Location = Int

type Env = Map.Map Name Location

type Store = Map.Map Location Value

data InterpException
  = NotFound
  | ArityError
  | TypeError
  deriving (Show)

data InterpState = InterpState
  { env :: Env,
    store :: Store,
    loc :: Int
  }

data Def
  = DVal Name Expr
  | DExpr Expr
  | DDefine Name [Name] Expr

type InterpMonad = StateT InterpState (Except InterpException)

data Expr
  = ELiteral Value
  | EVar Name
  | ESet Name Expr
  | EIfx Expr Expr Expr
  | EWhilex Expr Expr
  | EBegin [Expr]
  | EApply Expr [Expr]
  | ELetx LetFlavor [(Name, Expr)] Expr
  | ELambda [Name] Expr
  deriving (Show)

data LetFlavor = Let | LetRec | LetStar
  deriving (Show)

data Value
  = VSym Name
  | VNum Int
  | VBool Bool
  | VNil
  | VPair Value Value
  | VClosure [Name] Expr Env
  | VPrimitve Primitive

type Primitive = Expr -> [Value] -> InterpMonad Value

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
  show (VClosure {}) = "<closure>"
  show (VPrimitve _) = "<primitive>"
