module Core where

import Basic
import Type
import Kind

data Def
  = Val Name Exp
  | Valrec Name Exp
  | Data Name Kind [(Name, Type)]

data Exp
  = Literal Value
  | Var Name
  | VCon VCon
  | Begin [Exp]
  | Apply Exp [Exp]
  | Let [(Name, Exp)] Exp
  | Letrec [(Name, Exp)] Exp
  | Lambda [Name] Exp
  | Case Exp [Choice]

type Choice = (Pattern, Exp)

data Pattern
  = PVar Name
  | PApp VCon [Pattern]
  | Underscore

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
  show (ConVal "cons" [v, vs]) = consString v vs
    where
      consString :: Value -> Value -> String
      consString v vs = "(" ++ show v ++ tail vs ++ ")"
      tail :: Value -> String
      tail (ConVal "cons" [v, vs]) = " " ++ show v ++ tail vs
      tail (ConVal "'()" []) = ")"
      tail _ = error "BugInTypeInference: bad list constructor (or cons/'() redefined)"
  show (ConVal "'()" []) = "()"
  show (ConVal c []) = c
  show (ConVal c vs) = "(" ++ unwords (c : map show vs) ++ ")"
  show (Sym v) = v
  show (Num n) = show n
  show (Closure {}) = "<function>"
  show (Primitive _) = "<function>"
