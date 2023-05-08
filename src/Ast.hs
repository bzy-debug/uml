module Ast where

import Basic
import Kind
import Type

data Code
  = Command Command
  | Definition Def

data Command
  = Use Name
  | Check Exp Value

data Def
  = Val Name Exp
  | Valrec Name Exp
  | DExp Exp
  | Define Name [Name] Exp
  | Data Name Kind [(Name, TypeExp)]
  | Implicit [Name] Name [(Name, [TypeExp])]

data Exp
  = Literal Value
  | Var Name
  | VCon VCon
  | If Exp Exp Exp
  | Apply Exp [Exp]
  | Letx LetFlavor [(Name, Exp)] Exp
  | Lambda [Name] Exp
  | Case Exp [Choice]
  | Letp LetFlavor [Choice] Exp
  | Lambdap [Pattern] Exp

data LetFlavor = Let | LetRec | LetStar

type Choice = (Pattern, Exp)

data Pattern
  = PVar Name
  | PApp VCon [Pattern]
  | Underscore

data Value
  = Sym Name
  | Num Int
  | ConVal VCon [Value]
