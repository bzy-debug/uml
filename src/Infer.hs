{-# LANGUAGE TupleSections #-}

module Infer where

import Assumption
import Ast
import Constraint
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Scheme
import Subst
import Type

type TypeofMonad = ExceptT String (ReaderT Assumption (StateT Int Identity))

freshType :: TypeofMonad Type
freshType = do
  counter <- get
  let freshName = "'t" ++ show counter
  put $ counter + 1
  return $ TVar freshName

freshTypes :: Int -> TypeofMonad [Type]
freshTypes n = replicateM n freshType

cannotUnify :: Type -> Type -> TypeofMonad a
cannotUnify t t' =
  throwError $ "TypeError: cannot make " ++ show t ++ " equal to " ++ show t'

varBind :: Name -> Type -> TypeofMonad Subst
varBind x (TVar y) =
  return $
    if x == y
      then emptySubst
      else singleSubst x (TVar y)
varBind x t =
  if occurs x t
    then throwError $ "BugInTypeTypeofence: " ++ x ++ " occurs in " ++ show t
    else return $ singleSubst x t

solve :: Constraint -> TypeofMonad Subst
solve Trival = return emptySubst
solve (Cand c1 c2) = do
  s1 <- solve c1
  s2 <- solve $ substCons s1 c2
  return $ compose s2 s1
solve (Ceq t1 t2) =
  case (t1, t2) of
    (TVar a, t) ->
      if a `occurs` t
        then
          if t1 == t2
            then return emptySubst
            else cannotUnify t1 t2
        else varBind a t
    (t, TVar a) ->
      if a `occurs` t
        then
          if t1 == t2
            then return emptySubst
            else cannotUnify t1 t2
        else varBind a t
    (TCon c1, TCon c2) ->
      if c1 == c2
        then return emptySubst
        else cannotUnify t1 t2
    (TApp c1 ts1, TApp c2 ts2) ->
      if c1 == c2
        then solve $ conjoin (zipWith Ceq ts1 ts2)
        else cannotUnify t1 t2
    _ -> cannotUnify t1 t2

literalType :: Value -> TypeofMonad Type
literalType (Sym _) = return symType
literalType (Num _) = return intType
literalType (Bool _) = return boolType
literalType Nil = return $ listType alpha
literalType (Pair v Nil) = listType <$> literalType v
literalType (Pair v1 v2) = do
  t1 <- literalType v1
  t2 <- literalType v2
  if listType t1 == t2
    then return t2
    else cannotUnify (listType t1) t2
literalType _ = throwError "BugInTypeTypeofence: closures and primitives are not literal"

typeof :: Exp -> TypeofMonad (Type, Constraint)
typeof (Literal v) = (,Trival) <$> literalType v
typeof (Var x) = do
  assum <- ask
  case findScheme assum x of
    Nothing -> throwError $ "NotFound: " ++ x
    Just scheme@(Scheme names _) ->
      (,Trival) . instantiate scheme <$> freshTypes (length names)
typeof (If e1 e2 e3) = do
  (t1, c1) <- typeof e1
  (t2, c2) <- typeof e2
  (t3, c3) <- typeof e3
  return (t2, conjoin [c1, c2, c3, t1 `Ceq` boolType, t2 `Ceq` t3])
typeof (Begin es) =
  let iter [] last = last
      iter (e : es) _ = iter es (typeof e)
   in iter es (return (boolType, Trival))
typeof (Lambda names exp) = do
  alphas <- freshTypes (length names)
  let schemes = map (Scheme []) alphas
  (ret, c) <- local (bindSchemes names schemes) (typeof exp)
  return (funType alphas ret, c)
typeof (Apply fun args) = do
  (typs, c) <- typeofMany (fun : args)
  case typs of
    [] -> undefined
    (funTyp : argTyps) -> do
      alpha <- freshType
      return (alpha, c `Cand` (funTyp `Ceq` funType argTyps alpha))
typeof (Letx Let binds body) = do
  let (names, exps) = unzip binds
  (typs, c) <- typeofMany exps
  s <- solve c
  assum <- ask
  let c' = conjoin [TVar alpha `Ceq` subst s (TVar alpha) | alpha <- dom s `union` ftvAssum assum]
  let schemes = [generalize (subst s typ) (ftvAssum assum ++ ftvCons c') | typ <- typs]
  (bodyTyp, bodyCons) <- local (bindSchemes names schemes) (typeof body)
  return (bodyTyp, c' `Cand` bodyCons)
typeof (Letx LetStar binds body) =
  typeof $ desugur binds
  where
    desugur :: [(Name, Exp)] -> Exp
    desugur [] = body
    desugur (b : bs) = Letx Let [b] $ desugur bs
typeof (Letx LetRec binds body) = do
  let (names, exps) = unzip binds
  alphas <- freshTypes (length names)
  let alphaSchemes = map (Scheme []) alphas
  (typs, cr) <- local (bindSchemes names alphaSchemes) (typeofMany exps)
  let c = conjoin $ cr : zipWith Ceq typs alphas
  s <- solve c
  assum <- ask
  let c' = conjoin [TVar alpha `Ceq` subst s (TVar alpha) | alpha <- dom s `union` ftvAssum assum]
  let schemes = [generalize (subst s typ) (ftvAssum assum ++ ftvCons c') | typ <- typs]
  (bodyTyp, bodyCons) <- local (bindSchemes names schemes) (typeof body)
  return (bodyTyp, c' `Cand` bodyCons)

typeofMany :: [Exp] -> TypeofMonad ([Type], Constraint)
typeofMany exps = do
  typCons <- mapM typeof exps
  let (expTypes, expCons) = unzip typCons
  return (expTypes, conjoin expCons)

runTypeof :: TypeofMonad a -> (Either String a, Int)
runTypeof e = runIdentity (runStateT (runReaderT (runExceptT e) primitiveAssum) 0)

infer :: Exp -> TypeofMonad Scheme
infer exp = do
  (typ, con) <- typeof exp
  s <- solve con
  let typ' = subst s typ
  return $ generalize typ' []

infer' :: Exp -> Either String String
infer' exp = case fst (runTypeof (infer exp)) of
  Left err -> Left err
  Right v -> Right $ show v
