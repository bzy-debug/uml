{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Infer where

import Ast
import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Data.List
import Type

type InferMonad = StateT Int (Either String)

freshtyvar :: InferMonad Type
freshtyvar = do
  i <- get
  let tau = TVar $ "'t" ++ show i
  put $ i + 1
  return tau

freshtyvars :: Int -> InferMonad [Type]
freshtyvars n = replicateM n freshtyvar

isList :: Value -> Bool
isList Nil = True
isList (Pair _ v') = isList v'
isList _ = False

generalize :: Type -> [Name] -> TypScheme
generalize tau tyvars =
  canonicalize
    ( Forall
        ( freetyvars tau \\ tyvars
        )
        tau
    )

canonicalize :: TypScheme -> TypScheme
canonicalize (Forall bound ty) =
  let canonicalTyvarName n =
        if n < 26
          then ['\'', chr (ord 'a' + n)]
          else "'v" ++ show (n - 25)
      free = freetyvars ty \\ bound
      unusedIndex n =
        if canonicalTyvarName n `elem` free
          then unusedIndex (n + 1)
          else n
      newBoundVars _ [] = []
      newBoundVars index (_ : oldvars) =
        let n = unusedIndex index
         in canonicalTyvarName n : newBoundVars (n + 1) oldvars
      newBound = newBoundVars 0 bound
   in Forall newBound (tysubst (zip bound (map TVar newBound)) ty)

instantiate :: TypScheme -> [Type] -> InferMonad Type
instantiate (Forall formals tau) actuals =
  if length formals == length actuals
    then return $ tysubst (zip formals actuals) tau
    else throwError "InternalError"

freshInstance :: TypScheme -> InferMonad Type
freshInstance t@(Forall bound _) = do
  xs <- freshtyvars (length bound)
  instantiate t xs

type TypeEnv = (Env TypScheme, [Name])

emptyTypeEnv :: TypeEnv
emptyTypeEnv = ([], [])

findtyscheme :: Name -> TypeEnv -> InferMonad TypScheme
findtyscheme x (gamma, _) =
  case lookup x gamma of
    Nothing -> throwError "NotFound"
    Just scheme -> return scheme

bindtyscheme :: (Name, TypScheme) -> TypeEnv -> TypeEnv
bindtyscheme (x, sigma@(Forall bound tau)) (gamma, free) =
  ( (x, sigma) : gamma,
    (freetyvars tau \\ bound) `union` free
  )

freetyvarsGamma :: TypeEnv -> [Name]
freetyvarsGamma (_, free) = free

infix 4 :~

infixl 3 :/\

data Con
  = (:~) Type Type
  | (:/\) Con Con
  | Trivial

freetyvarsCon :: Con -> [Name]
freetyvarsCon (t :~ t') = freetyvars t `union` freetyvars t'
freetyvarsCon (c :/\ c') = freetyvarsCon c `union` freetyvarsCon c'
freetyvarsCon Trivial = []

consubst :: Subst -> Con -> Con
consubst theta (tau1 :~ tau2) = tysubst theta tau1 :~ tysubst theta tau2
consubst theta (c1 :/\ c2) = consubst theta c1 :/\ consubst theta c2
consubst _ Trivial = Trivial

conjoinCons :: [Con] -> Con
conjoinCons = foldr (:/\) Trivial

unstaisfiableEq :: Type -> Type -> a
unstaisfiableEq = undefined

solve :: Con -> InferMonad Subst
solve Trivial = return []
solve (c1 :/\ c2) = do
  s1 <- solve c1
  s2 <- solve $ consubst s1 c2
  return $ compose s1 s2
solve (t1 :~ t2) =
  case (t1, t2) of
    (TVar x, t2) ->
      if x `elem` freetyvars t2
        then case t2 of
          TVar y | x == y -> return []
          _ -> throwError "cannot unify"
        else return $ x |--> t2
    (t1, TVar _) -> solve (t2 :~ t1)
    (TCon c, TCon c') ->
      if c == c'
        then return []
        else throwError "cannot unify"
    (ConApp t ts, ConApp t' ts') ->
      let con = conjoinCons $ (t :~ t') : zipWith (:~) ts ts'
       in solve con
    _ -> throwError "cannot unify"

isSolved :: Con -> Bool
isSolved Trivial = True
isSolved (t :~ t') = t == t'
isSolved (c :/\ c') = isSolved c && isSolved c'

solves :: Subst -> Con -> Bool
solves theta c = isSolved $ consubst theta c

typeof :: Exp -> TypeEnv -> InferMonad (Type, Con)
typeof e gamma =
  let typesof :: [Exp] -> TypeEnv -> InferMonad ([Type], Con)
      typesof [] _ = return ([], Trivial)
      typesof (e : es) gamma = do
        (tau, c) <- typeof e gamma
        (taus, c') <- typesof es gamma
        return (tau : taus, c :/\ c')
      literal :: Value -> InferMonad Type
      literal (Sym _) = return symType
      literal (Num _) = return intType
      literal (Bool _) = return boolType
      literal Nil = return $ listType alpha
      literal val@(Pair v v')
        | isList val = do
            t <- literal v
            case v' of
              Nil -> return $ listType t
              _ -> do
                t' <- literal v'
                if t' == listType t
                  then return $ listType t
                  else throwError "list type not consist"
      literal (Pair v v') = do
        t <- literal v
        t' <- literal v'
        return $ pairType t t'
      literal _ = throwError "undefined"
      ty :: Exp -> InferMonad (Type, Con)
      ty (Literal v) = literal v >>= \t -> return (t, Trivial)
      ty (Var x) = do
        scheme <- findtyscheme x gamma
        typ <- freshInstance scheme
        return (typ, Trivial)
      ty (Apply f actuals) = do
        types <- typesof (f : actuals) gamma
        case types of
          ([], _) -> throwError "pattern match"
          (funty : actualtypes, c) -> do
            rettype <- freshtyvar
            return (rettype, c :/\ (funty :~ funType actualtypes rettype))
      ty (Letx LetStar [] body) = ty body
      ty (Letx LetStar (b : bs) body) =
        ty (Letx Let [b] (Letx LetStar bs body))
      ty (If e1 e2 e3) = do
        (t1, c1) <- ty e1
        (t2, c2) <- ty e2
        (t3, c3) <- ty e3
        return (t2, c1 :/\ c2 :/\ c3 :/\ t1 :~ boolType :/\ t2 :~ t3)
      ty (Begin exprs) = do
        typ <- iter exprs boolType
        return (typ, Trivial)
        where
          iter [] tau = return tau
          iter (e : es) _ = ty e >>= \(t, _) -> iter es t
      ty (Lambda names body) = do
        alphas <- freshtyvars (length names)
        let schemes = map (Forall []) alphas
        let gamma' = foldr bindtyscheme gamma (zip names schemes)
        (tau, c) <- typeof body gamma'
        return (funType alphas tau, c)
      ty (Letx Let bs body) = do
        let (xs, es) = unzip bs
        (ts, c) <- typesof es gamma
        theta <- solve c
        let c' = conjoinCons [TVar a :~ tysubst theta (TVar a) | a <- dom theta `intersect` freetyvarsGamma gamma]
        let schemes = [generalize (tysubst theta t) (freetyvarsGamma gamma `union` freetyvarsCon c') | t <- ts]
        (tau, cb) <- typeof body (foldr bindtyscheme gamma (zip xs schemes))
        return (tau, cb :/\ c')
      ty (Letx LetRec bs body) = do
        let (xs, es) = unzip bs
        alphas <- freshtyvars (length xs)
        let schemes = map (Forall []) alphas
        let gamma' = foldr bindtyscheme gamma (zip xs schemes)
        (taus, cr) <- typesof es gamma'
        let c = conjoinCons (cr : zipWith (:~) taus alphas)
        theta <- solve c
        let c' = conjoinCons [TVar a :~ tysubst theta (TVar a) | a <- dom theta `intersect` freetyvarsGamma gamma]
        let schemes' = [generalize tau (freetyvarsGamma gamma `union` freetyvarsCon c') | tau <- taus]
        (tau, cb) <- typeof body (foldr bindtyscheme gamma (zip xs schemes'))
        return (tau, c' :/\ cb)
   in ty e