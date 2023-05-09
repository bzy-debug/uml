module Core where

import Basic
import Kind
import Type

data Def
  = Val Name Exp
  | Valrec Name Exp
  | Data Name Kind [(Name, TypeExp)]
  deriving (Show)

data Exp
  = Literal Value
  | Var Name
  | VCon VCon
  | Apply Exp [Exp]
  | Let [(Name, Exp)] Exp
  | Letrec [(Name, Exp)] Exp
  | Lambda [Name] Exp
  | Case Exp [Choice]
  deriving (Show)

type Choice = (Pattern, Exp)

data Pattern
  = PVar Name
  | PApp VCon [Pattern]
  | Underscore
  deriving (Show)

data Value
  = Sym Name
  | Num Int
  | ConVal VCon [Value]
  | Closure ([Name], Exp) Ref
  | Primitive Primitive

instance Eq Value where
  (Sym n) == (Sym n') = n == n'
  (Num i) == (Num i') = i == i'
  (ConVal c vs) == (ConVal c' vs') =
    (c == c') && and (zipWith (==) vs vs')
  _ == _ = False

type Primitive = [Value] -> Value

instance Show Value where
  show (ConVal "CONS" [v, vs]) = consString v vs
    where
      consString :: Value -> Value -> String
      consString v vs = "(" ++ show v ++ tail vs
      tail :: Value -> String
      tail (ConVal "CONS" [v, vs]) = " " ++ show v ++ tail vs
      tail (ConVal "NIL" []) = ")"
      tail _ = error "BugInTypeInference: bad list constructor (or CONS/NIL redefined)"
  show (ConVal "NIL" []) = "()"
  show (ConVal c []) = c
  show (ConVal c vs) = "(" ++ unwords (c : map show vs) ++ ")"
  show (Sym v) = v
  show (Num n) = show n
  show (Closure {}) = "<function>"
  show (Primitive _) = "<function>"
