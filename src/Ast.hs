module Ast where

import Basic
import Kind
import Type

type Ref = Int

data Code
  = Command Command
  | Definition Def
  | Expression Exp

data Command
  = Use Name
  | Check Exp Value

data Def
  = Val Name Exp
  | Valrec Name Exp
  | DExp Exp
  | Define Name [Name] Exp
  | Data Name Kind [(Name, Type)]

data Exp
  = Literal Value
  | Var Name
  | VCon VCon
  | If Exp Exp Exp
  | Begin [Exp]
  | Apply Exp [Exp]
  | Letx LetFlavor [(Name, Exp)] Exp
  | Lambda [Name] Exp
  | -- | Letp LetFlavor [Choice] Exp
    -- | Lambdap [Pattern] Exp
    Case Exp [Choice]

data LetFlavor = Let | LetRec | LetStar

type Choice = (Pattern, Exp)

data Pattern
  = PVar Name
  | PApp VCon [Pattern]
  | Underscore

instance Show Pattern where
  show p =
    case p of
      PVar n -> n
      PApp n p -> parenSpace $ n : map show p
      Underscore -> "_"
    where
      paren s = "(" ++ s ++ ")"
      parenSpace = paren . unwords

instance Show Exp where
  show e =
    case e of
      Literal v -> show v
      Var x -> x
      VCon c -> c
      If e1 e2 e3 -> parenSpace $ "if" : exps [e1, e2, e3]
      Begin es -> parenSpace $ "begin" : exps es
      Apply e es -> parenSpace $ exps (e : es)
      Letx lk bs e -> parenSpace [show lk, bindings bs, show e]
      Lambda xs body -> parenSpace ["lambda", bracketSpace xs, show body]
      -- Letp lk cs e -> parenSpace [show lk, choices cs, show e]
      -- Lambdap ps e -> parenSpace ["lambda", bracketSpace (patterns ps), show e]
      Case scrutinee cs -> parenSpace [show scrutinee, choices cs]
    where
      paren s = "(" ++ s ++ ")"
      bracket s = "[" ++ s ++ "]"
      parenSpace = paren . unwords
      bracketSpace = bracket . unwords
      exps = map show
      --      patterns = map (show :: Pattern -> String)
      choice (p, e) = bracketSpace [(show :: Pattern -> String) p, (show :: Exp -> String) e]
      choices = bracketSpace . map choice
      binding (x, e) = bracket $ x ++ " " ++ show e
      bindings = parenSpace . map binding

instance Show LetFlavor where
  show Let = "let"
  show LetRec = "letrec"
  show LetStar = "let*"

type Primitive = [Value] -> Value

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
