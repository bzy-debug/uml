module Ast where

import Basic
import Kind

data Code
  = Command Command
  | Definition Def
  | Expression Exp

data Command
  = Use Name
  | Check Exp Value

data TypeExp
  = TyName Name
  | ConApp TypeExp [TypeExp]
  | FunTy [TypeExp] TypeExp
  | Forall [Name] TypeExp
  | TyVar Name

data Def
  = Val Name Exp
  | Valrec Name Exp
  | DExp Exp
  | Define Name [Name] Exp
  | Data Name Kind [(Name, TypeExp)]
  | Implicit (Maybe [Name]) Name [(Name, Maybe TypeExp)]

data Exp
  = Literal Value
  | Var Name
  | VCon VCon
  | If Exp Exp Exp
  | Begin [Exp]
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

-- instance Show Pattern where
--   show p =
--     case p of
--       PVar n -> n
--       PApp n p -> parenSpace $ n : map show p
--       Underscore -> "_"
--     where
--       paren s = "(" ++ s ++ ")"
--       parenSpace = paren . unwords

-- instance Show Exp where
--   show e =
--     case e of
--       Literal v -> show v
--       Var x -> x
--       VCon c -> c
--       If e1 e2 e3 -> parenSpace $ "if" : exps [e1, e2, e3]
--       Begin es -> parenSpace $ "begin" : exps es
--       Apply e es -> parenSpace $ exps (e : es)
--       Letx lk bs e -> parenSpace [show lk, bindings bs, show e]
--       Lambda xs body -> parenSpace ["lambda", bracketSpace xs, show body]
--       Letp lk cs e -> parenSpace [show lk, choices cs, show e]
--       Lambdap ps e -> parenSpace ["lambda", bracketSpace (patterns ps), show e]
--       Case scrutinee cs -> parenSpace [show scrutinee, choices cs]
--     where
--       paren s = "(" ++ s ++ ")"
--       bracket s = "[" ++ s ++ "]"
--       parenSpace = paren . unwords
--       bracketSpace = bracket . unwords
--       exps = map show
--       patterns = map (show :: Pattern -> String)
--       choice (p, e) = bracketSpace [(show :: Pattern -> String) p, (show :: Exp -> String) e]
--       choices = bracketSpace . map choice
--       binding (x, e) = bracket $ x ++ " " ++ show e
--       bindings = parenSpace . map binding

-- instance Show LetFlavor where
--   show Let = "let"
--   show LetRec = "letrec"
--   show LetStar = "let*"

-- type Primitive = [Value] -> Value

-- data Value
--   = Sym Name
--   | Num Int
--   | ConVal VCon [Value]
