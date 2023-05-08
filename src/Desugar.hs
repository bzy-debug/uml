module Desugar where

import qualified Ast
import Basic
import qualified Core
import Data.Bifunctor
import Data.List
import Type
import Kind

freeVarPat :: Ast.Pattern -> [Name]
freeVarPat (Ast.PVar x) = [x]
freeVarPat (Ast.PApp vcon pats) = foldr (union . freeVarPat) [] pats `union` [vcon]
freeVarPat Ast.Underscore = []

freeVarChoice :: Ast.Choice -> [Name]
freeVarChoice (pat, exp) = freeVars exp \\ freeVarPat pat

freeVars :: Ast.Exp -> [Name]
freeVars (Ast.Literal _) = []
freeVars (Ast.Var x) = [x]
freeVars (Ast.VCon c) = [c]
freeVars (Ast.If e1 e2 e3) = freeVars e1 `union` freeVars e2 `union` freeVars e3
freeVars (Ast.Apply rator rands) = freeVars rator `union` foldr (union . freeVars) [] rands
freeVars (Ast.Letx _ bindings body) =
  let (bound, _) = unzip bindings
   in freeVars body \\ bound
freeVars (Ast.Lambda xs body) = freeVars body \\ xs
freeVars (Ast.Case scrutinee choices) = freeVars scrutinee `union` foldr (union . freeVarChoice) [] choices
freeVars (Ast.Letp _ choices body) = freeVars body \\ foldr (union . freeVarChoice) [] choices
freeVars (Ast.Lambdap pats body) = freeVars body \\ foldr (union . freeVarPat) [] pats

freshVar :: Ast.Exp -> Name
freshVar exp = helper 0 (freeVars exp)
  where
    helper :: Int -> [Name] -> Name
    helper i ns =
      let new = "x!" ++ show i
       in if new `elem` ns
            then helper (i + 1) ns
            else new

freshVars :: Ast.Exp -> Int -> [Name]
freshVars exp n = helper 0 [] (freeVars exp)
  where
    helper :: Int -> [Name] -> [Name] -> [Name]
    helper counter news names =
      if length news == n
        then news
        else
          let new = "x!" ++ show counter
           in if new `elem` names || new `elem` news
                then helper (counter + 1) news names
                else helper (counter + 1) (new : news) names

convertValue :: Ast.Value -> Core.Value
convertValue (Ast.Sym n) = Core.Sym n
convertValue (Ast.Num i) = Core.Num i
convertValue (Ast.ConVal vcon vs) = Core.ConVal vcon (map convertValue vs)

convertPat :: Ast.Pattern -> Core.Pattern
convertPat (Ast.PVar n) = Core.PVar n
convertPat (Ast.PApp vcon pats) = Core.PApp vcon (map convertPat pats)
convertPat Ast.Underscore = Core.Underscore

convertChoice :: Ast.Choice -> Core.Choice
convertChoice (pat, exp) = (convertPat pat, desugar exp)

desugar :: Ast.Exp -> Core.Exp
desugar (Ast.Literal v) = Core.Literal (convertValue v)
desugar (Ast.Var x) = Core.Var x
desugar (Ast.VCon vcon) = Core.VCon vcon
desugar (Ast.If e1 e2 e3) =
  let c2 = (Core.PApp "#t" [], desugar e2)
      c3 = (Core.PApp "#f" [], desugar e3)
   in Core.Case (desugar e1) [c2, c3]
desugar (Ast.Apply rator rands) = Core.Apply (desugar rator) (map desugar rands)
desugar (Ast.Letx Ast.Let bindings body) = Core.Let (map (second desugar) bindings) (desugar body)
desugar (Ast.Letx Ast.LetStar bindings body) =
  case bindings of
    [] -> Core.Let [] (desugar body)
    b : bs -> Core.Let [second desugar b] (desugar (Ast.Letx Ast.LetStar bs body))
desugar (Ast.Letx Ast.LetRec bindings body) = Core.Letrec (map (second desugar) bindings) (desugar body)
desugar (Ast.Lambda xs body) = Core.Lambda xs (desugar body)
desugar (Ast.Case scrutinee choices) = Core.Case (desugar scrutinee) (map convertChoice choices)
desugar (Ast.Lambdap [pat] body) =
  let x = freshVar body
   in Core.Lambda [x] (Core.Case (Core.Var x) [(convertPat pat, desugar body)])
desugar (Ast.Lambdap _ _) = error "TODO: more tuple"
desugar (Ast.Letp {}) = error "TODO: desugar letp"

desugarDef :: Ast.Def -> Core.Def
desugarDef (Ast.Val name exp) = Core.Val name (desugar exp)
desugarDef (Ast.Valrec name exp) = Core.Valrec name (desugar exp)
desugarDef (Ast.DExp exp) = Core.Val "it" (desugar exp)
desugarDef (Ast.Define name formals body) = Core.Valrec name (Core.Lambda formals (desugar body))
desugarDef (Ast.Data name kind entries) = Core.Data name kind entries
desugarDef (Ast.Implicit [] t vcons) =
  let tx = TyCon t
      convertVcon (k, []) = (k, tx)
      convertVcon (k, txs) = (k, FunTy txs tx)
   in Core.Data t Star (map convertVcon vcons)
desugarDef (Ast.Implicit alphas t vcons) =
  let kind = Arrow (map (const Star) alphas) Star
      tx = ConApp (TyCon t) (map TyVar alphas)
      close = Forall alphas
      convertVcon (k, []) = (k, close tx)
      convertVcon (k, txs) = (k, close (FunTy txs tx))
   in Core.Data t kind (map convertVcon vcons)
