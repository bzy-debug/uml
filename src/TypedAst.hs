module TypedAst where

import Data.Map as Map

type Name = String

type Env a = Map.Map Name a

data Kind
  = Type
  | Arrow [Kind] Kind
  deriving (Eq, Show)

data TExpr
  = TCons Name
  | TApp TExpr [TExpr]
  | TFun [TExpr] TExpr
  | TForall [Name] TExpr
  | TVar Name
  deriving (Show)

data Value
  = VSym Name
  | VNum Int
  | VBool Bool
  | VNil
  | VPair Value Value
  | VClosure [Name] Expr (Env Value)

data Expr
  = Literal Value
  | Var Name
  | If Expr Expr Expr
  | While Expr Expr
  | Begin [Expr]
  | Apply Expr [Expr]
  | Let [(Name, Expr)] Expr
  | Letrec [(Name, TExpr, Expr)] Expr
  | Lambda [(Name, TExpr)] Expr
  | TLambda [Name] Expr
  | TApply Expr [TExpr]

data Define
  = Val Name Expr
  | Valrec Name TExpr Expr
  | Expr Expr
  | Define Name TExpr [(Name, TExpr)] Expr

delta0 :: Env Kind
delta0 =
  Map.fromList
    [ ("int", Type),
      ("bool", Type),
      ("char", Type),
      ("pair", Arrow [Type, Type] Type),
      ("sum", Arrow [Type, Type] Type),
      ("list", Arrow [Type] Type)
    ]

typeof :: Expr -> Env Kind -> Env TExpr -> TExpr
typeof = undefined
