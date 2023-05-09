module Ast where

import Basic
import Kind
import Type

data Code
  = Command Command
  | Definition Def
  deriving Show

data Command
  = Use Name
 | Check Exp Exp
  deriving Show

data Def
  = Val Name Exp
  | Valrec Name Exp
  | DExp Exp
  | Define Name [Name] Exp
  | DefineS [Clause]
  | Data Name Kind [(Name, TypeExp)]
  | Implicit [Name] Name [(Name, [TypeExp])]
  deriving Show

type Clause = (Name, [Pattern], Exp)

data Exp
  = Literal Value
  | Var Name
  | VCon VCon
  | If Exp Exp Exp
  | Apply Exp [Exp]
  | Letx LetFlavor [Choice] Exp
  | Lambda [Pattern] Exp
  | LambdaS [Branch]
  | Case Exp [Choice]
  deriving Show

type Branch = ([Pattern], Exp)

data LetFlavor = Let | LetRec | LetStar
  deriving Show

type Choice = (Pattern, Exp)

data Pattern
  = PVar Name
  | PApp VCon [Pattern]
  | Underscore
  deriving Show

data Value
  = Sym Name
  | Num Int
  | ConVal VCon [Value]
  deriving Show
