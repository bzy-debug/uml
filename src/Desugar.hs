module Desugar where

import qualified Ast
import Basic
import qualified Core
import Data.List
import Kind
import Type

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
freeVars (Ast.Case scrutinee choices) = freeVars scrutinee `union` foldr (union . freeVarChoice) [] choices
freeVars (Ast.Letx _ choices body) = freeVars body \\ foldr (union . freeVarChoice) [] choices
freeVars (Ast.Lambda pats body) = freeVars body \\ foldr (union . freeVarPat) [] pats
freeVars (Ast.LambdaS branches) =
  unions $ map (\(pats, body) -> freeVars body \\ foldr (union . freeVarPat) [] pats) branches

freshVar :: Int -> [Name] -> [Name] -> Int -> [Name]
freshVar counter news names n =
  if length news == n
    then news
    else
      let new = "x_" ++ show counter
       in if new `elem` names || new `elem` news
            then freshVar (counter + 1) news names n
            else freshVar (counter + 1) (new : news) names n

freshVarExp :: Ast.Exp -> Int -> [Name]
freshVarExp exp = freshVar 0 [] (freeVars exp)

freshVarExps :: [Ast.Exp] -> Int -> [Name]
freshVarExps exps = freshVar 0 [] (concatMap freeVars exps)

convertValue :: Ast.Value -> Core.Value
convertValue (Ast.Sym n) = Core.Sym n
convertValue (Ast.Num i) = Core.Num i
convertValue (Ast.ConVal vcon vs) =
  Core.ConVal vcon (map convertValue vs)

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
desugar (Ast.Lambda pats body) =
  let xs = freshVarExp body (length pats)
   in Core.Lambda
        xs
        ( Core.Case
            (desugar $ tupleExp xs)
            [(convertPat $ tuplePats pats, desugar body)]
        )
desugar (Ast.LambdaS branches) =
  let (patss, exps) = unzip branches
      xs = freshVarExps exps (length $ head patss)
      scrutinee = tupleExp xs
      choices = [(tuplePats pats, exp) | (pats, exp) <- zip patss exps]
      lambdaBody = Ast.Case scrutinee choices
   in Core.Lambda xs (desugar lambdaBody)
desugar (Ast.Letx Ast.LetStar choices body) =
  let helper [] body = Ast.Letx Ast.Let [] body
      helper (c : cs) body = Ast.Letx Ast.Let [c] (helper cs body)
   in desugar (helper choices body)
desugar (Ast.Letx Ast.Let choices body) =
  let xs = freshVarExp body (length choices)
      helper (p, e) x =
        let cp = convertPat p
            ys = freePatVars cp
            ybindings = [(y, Core.Case (Core.Var x) [(cp, Core.Var y)]) | y <- ys]
         in ((x, desugar e), ybindings)
      (xbindings, ybindingss) = unzip $ zipWith helper choices xs
   in Core.Let xbindings (Core.Let (concat ybindingss) (desugar body))
desugar (Ast.Letx Ast.LetRec choices body) =
  let asBinding (Ast.PVar x, exp) = (x, desugar exp)
      asBinding _ = error "BugInParser: let rec only binds variable"
   in Core.Letrec (map asBinding choices) (desugar body)
desugar (Ast.Case scrutinee choices) = Core.Case (desugar scrutinee) (map convertChoice choices)

tuplePats :: [Ast.Pattern] -> Ast.Pattern
tuplePats [pat] = pat
tuplePats pats = Ast.PApp (tupleVCon pats) pats

tupleExp :: [Name] -> Ast.Exp
tupleExp [x] = Ast.Var x
tupleExp xs =
  Ast.Apply
    (Ast.VCon (tupleVCon xs))
    (map Ast.Var xs)

tupleVCon :: [a] -> VCon
tupleVCon xs =
  case length xs of
    2 -> "PAIR"
    3 -> "TRIPLE"
    n -> "T" ++ show n

freePatVars :: Core.Pattern -> [Name]
freePatVars (Core.PVar x) = [x]
freePatVars (Core.PApp _ pats) = unions (map freePatVars pats)
freePatVars Core.Underscore = []

desugarDef :: Ast.Def -> Core.Def
desugarDef (Ast.Val name exp) = Core.Val name (desugar exp)
desugarDef (Ast.Valrec name exp) = Core.Valrec name (desugar exp)
desugarDef (Ast.DExp exp) = Core.Val "it" (desugar exp)
desugarDef (Ast.Define name formals body) = Core.Valrec name (Core.Lambda formals (desugar body))
desugarDef (Ast.DefineS clauses) =
  let (names, patss, exps) = unzip3 clauses
      xs = freshVarExps exps (length $ head patss)
      name = head names
      scrutinee = tupleExp xs
      choices = [(tuplePats pats, exp) | (pats, exp) <- zip patss exps]
      defBody = Ast.Case scrutinee choices
   in desugarDef (Ast.Define name xs defBody)
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
