{-# LANGUAGE TupleSections #-}

module Infer where

import Assumption
import Ast
import Basic
import Constraint
import Control.Monad.Except
import Control.Monad.State
import Data.List
import Kind
import Lens.Micro
import Lens.Micro.Extras
import Scheme
import Subst
import Type

type TypeofMonad = ExceptT String (State TypeofState)

data TypeofState = TypeofState
  { _nextIdentity :: Int,
    _freshCounter :: Int,
    _kindEnv :: Env (TypeCons, Kind),
    _typeEnv :: Assumption
  }

nextIdentity :: Lens' TypeofState Int
nextIdentity f (TypeofState i c k t) = (\i -> TypeofState i c k t) <$> f i

freshCounter :: Lens' TypeofState Int
freshCounter f (TypeofState i c k t) = (\c -> TypeofState i c k t) <$> f c

kindEnv :: Lens' TypeofState (Env (TypeCons, Kind))
kindEnv f (TypeofState i c k t) = (\k -> TypeofState i c k t) <$> f k

typeEnv :: Lens' TypeofState Assumption
typeEnv f (TypeofState i c k t) = TypeofState i c k <$> f t

getAssum :: TypeofMonad Assumption
getAssum = gets $ view typeEnv

bindAssum :: Name -> Scheme -> TypeofMonad ()
bindAssum x s = modify $ typeEnv %~ bindScheme x s

bindsAssum :: [Name] -> [Scheme] -> TypeofMonad ()
bindsAssum xs ss = modify $ typeEnv %~ bindSchemes xs ss

localAssum :: (Assumption -> Assumption) -> TypeofMonad a -> TypeofMonad a
localAssum f mv = do
  initState <- get
  modify $ typeEnv %~ f
  v <- mv
  put initState
  return v

newConstructor :: String -> TypeofMonad TypeCons
newConstructor name = do
  new <- gets $ view nextIdentity
  modify $ nextIdentity %~ (+ 1)
  return $ TypeCons name new

freshType :: TypeofMonad Type
freshType = do
  typeofState <- get
  let counter = typeofState ^. freshCounter
  let freshName = "'t" ++ show counter
  put $ typeofState & freshCounter %~ (+ 1)
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
    then error $ "BugInTypeTypeofence: " ++ x ++ " occurs in " ++ show t
    else return $ singleSubst x t

solve :: Constraint -> TypeofMonad Subst
solve Trival = return emptySubst
solve (Cand c1 c2) = do
  s1 <- solve c1
  s2 <- solve $ substCons s1 c2
  return $ compose s2 s1
solve (Ceq t1 t2) =
  if t1 == t2
    then return emptySubst
    else case (t1, t2) of
      (TVar a, t) ->
        if a `occurs` t
          then cannotUnify t1 t2
          else varBind a t
      (t, TVar a) ->
        if a `occurs` t
          then cannotUnify t1 t2
          else varBind a t
      (TCon c1, TCon c2) ->
        if c1 == c2
          then return emptySubst
          else cannotUnify t1 t2
      (TApp c1 ts1, TApp c2 ts2) ->
        if c1 == c2 && length ts1 == length ts2
          then solve $ conjoin (zipWith Ceq ts1 ts2)
          else cannotUnify t1 t2
      _ -> cannotUnify t1 t2

literalType :: Value -> TypeofMonad Type
literalType (Sym _) = return symType
literalType (Num _) = return intType
literalType (ConVal "'()" []) = return $ listType alpha
literalType (ConVal "cons" [v, ConVal "'()" []]) = listType <$> literalType v
literalType (ConVal "cons" [v1, v2]) = do
  t1 <- literalType v1
  t2 <- literalType v2
  if listType t1 == t2
    then return t2
    else cannotUnify (listType t1) t2
literalType _ = error "BugInTypeTypeofence: closures and primitives are not literal"

typeof :: Exp -> TypeofMonad (Type, Constraint)
typeof (Literal v) = (,Trival) <$> literalType v
typeof (Var x) = do
  assum <- gets $ view typeEnv
  case findScheme assum x of
    Nothing -> throwError $ "NotFound: " ++ x
    Just scheme@(Scheme names _) ->
      (,Trival) . instantiate scheme <$> freshTypes (length names)
typeof (VCon c) = do
  assum <- gets $ view typeEnv
  case findScheme assum c of
    Nothing -> throwError $ "NotFound: " ++ c
    Just scheme@(Scheme names _) ->
      (,Trival) . instantiate scheme <$> freshTypes (length names)
typeof (If e1 e2 e3) = do
  (t1, c1) <- typeof e1
  (t2, c2) <- typeof e2
  (t3, c3) <- typeof e3
  return (t2, conjoin [c1, c2, c3, t1 `Ceq` boolType, t2 `Ceq` t3])
typeof (Begin es) =
  let iter [] last = last
      iter (e : es) _ = iter es $ do
        (t, c) <- typeof e
        _ <- solve c
        return (t, c)
   in iter es (return (boolType, Trival))
typeof (Lambda names exp) = do
  alphas <- freshTypes (length names)
  let schemes = map (Scheme []) alphas
  (ret, c) <- localAssum (bindSchemes names schemes) (typeof exp)
  return (funType alphas ret, c)
typeof (Apply fun args) = do
  (typs, c) <- typeofMany (fun : args)
  case typs of
    [] -> error "BugsInTypeInfer"
    (funTyp : argTyps) -> do
      alpha <- freshType
      return (alpha, c `Cand` (funTyp `Ceq` funType argTyps alpha))
typeof (Letx Let binds body) = do
  let (names, exps) = unzip binds
  (typs, c) <- typeofMany exps
  s <- solve c
  assum <- getAssum
  let c' = conjoin [TVar alpha `Ceq` subst s (TVar alpha) | alpha <- dom s `union` ftvAssum assum]
  let schemes = [generalize (subst s typ) (ftvAssum assum ++ ftvCons c') | typ <- typs]
  (bodyTyp, bodyCons) <- localAssum (bindSchemes names schemes) (typeof body)
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
  (typs, cr) <- localAssum (bindSchemes names alphaSchemes) (typeofMany exps)
  let c = conjoin $ cr : zipWith Ceq typs alphas
  s <- solve c
  assum <- getAssum
  let c' = conjoin [TVar alpha `Ceq` subst s (TVar alpha) | alpha <- dom s `union` ftvAssum assum]
  let schemes = [generalize (subst s typ) (ftvAssum assum ++ ftvCons c') | typ <- typs]
  (bodyTyp, bodyCons) <- localAssum (bindSchemes names schemes) (typeof body)
  return (bodyTyp, c' `Cand` bodyCons)
typeof (Case e choices) = do
  (te, ce) <- typeof e
  choicesTyps <- mapM typeofChoice choices
  let (tis, cis) = unzip choicesTyps
  alpha <- freshType
  let c' = conjoin $ map (\ti -> Ceq ti (funType [te] alpha)) tis
  let c = ce `Cand` c' `Cand` conjoin cis
  return (alpha, c)

typeofChoice :: Choice -> TypeofMonad (Type, Constraint)
typeofChoice (p, e) =
  do
    (binds, t, c) <- typeofPat p
    (t', c') <- localAssum (extendAssum binds) (typeof e)
    return (funType [t] t', Cand c c')

typeofPat :: Pattern -> TypeofMonad (Env Scheme, Type, Constraint)
typeofPat (PApp c []) = do
  t <- typeofVCon c
  return (emptyEnv, t, Trival)
typeofPat Underscore = do
  alpha <- freshType
  return (emptyEnv, alpha, Trival)
typeofPat (PVar x) = do
  alpha <- freshType
  let env = bind x (Scheme [] alpha) emptyEnv
  return (env, alpha, Trival)
typeofPat (PApp vcon pats) = do
  vconT <- typeofVCon vcon
  patsT <- mapM typeofPat pats
  let (envis, tis, cis) = unzip3 patsT
  alpha <- freshType
  let c = Ceq vconT (funType tis alpha)
  let c' = conjoin cis
  case disjointUnion envis of
    Left x -> throwError $ "TypeError: name " ++ x ++ " is bound multiple times in pattern"
    Right env' -> return (env', alpha, Cand c c')

typeofVCon :: VCon -> TypeofMonad Type
typeofVCon c = do
  assum <- gets $ view typeEnv
  case findScheme assum c of
    Nothing -> throwError $ "NotFound: " ++ c
    Just scheme@(Scheme names _) ->
      instantiate scheme <$> freshTypes (length names)

typeofMany :: [Exp] -> TypeofMonad ([Type], Constraint)
typeofMany exps = do
  typCons <- mapM typeof exps
  let (expTypes, expCons) = unzip typCons
  return (expTypes, conjoin expCons)

runTypeof :: TypeofMonad a -> TypeofState -> (Either String a, TypeofState)
runTypeof e = runState (runExceptT e)

infer :: Exp -> TypeofMonad Scheme
infer exp = do
  (typ, con) <- typeof exp
  s <- solve con
  let typ' = subst s typ
  return $ generalize typ' []

infer' :: Exp -> Either String String
infer' exp = case fst (runTypeof (infer exp) undefined) of
  Left err -> Left err
  Right v -> Right $ show v
