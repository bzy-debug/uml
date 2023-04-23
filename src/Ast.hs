module Ast where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Lens.Micro

type Name = String

type Env a = [(Name, a)]

emptyEnv :: Env a
emptyEnv = []

bind :: Name -> a -> Env a -> Env a
bind x v env = (x, v) : env

binds :: [Name] -> [a] -> Env a -> Env a
binds names vals env = zip names vals ++ env

type Ref = Int

data EnvRef = EnvRef
  { _mem :: [(Ref, Env Value)],
    _ref :: Ref
  }

data EvalState = EvalState
  { _valueEnv :: Env Value,
    _envRef :: EnvRef
  }

valueEnv :: Lens' EvalState (Env Value)
valueEnv f (EvalState e r) = (`EvalState` r) <$> f e

envRef :: Lens' EvalState EnvRef
envRef f (EvalState e r) = EvalState e <$> f r

mem :: Lens' EnvRef [(Ref, Env Value)]
mem f (EnvRef m r) = (`EnvRef` r) <$> f m

ref :: Lens' EnvRef Ref
ref f (EnvRef m r) = EnvRef m <$> f r

type EvalMonad = ExceptT String (StateT EvalState Identity)

type Prog = ([Def], [Exp])

data Def
  = Val Name Exp
  | Valrec Name Exp
  | DExp Exp
  | Define Name [Name] Exp

data Exp
  = Literal Value
  | Var Name
  | If Exp Exp Exp
  | Begin [Exp]
  | Apply Exp [Exp]
  | Letx LetFlavor [(Name, Exp)] Exp
  | Lambda [Name] Exp

data LetFlavor = Let | LetRec | LetStar

newtype Choice = Choice [(Pattern, Exp)]

data Pattern
  = PVar Name
  | PCon Name
  | PApp Name [Pattern]
  | Underscore

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

type Primitive = [Value] -> EvalMonad Value

data Value
  = Sym Name
  | Num Int
  | Bool Bool
  | Nil
  | Pair Value Value
  | Closure ([Name], Exp) Ref
  | Primitive Primitive

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
