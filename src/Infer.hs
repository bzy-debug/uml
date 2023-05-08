{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Infer where

import Basic
import Constraint
import Control.Monad.Except
import Control.Monad.State
import Core
import Data.List
import Kind
import Lens.Micro
import Lens.Micro.Extras
import Primitives
import Scheme
import Subst
import Text.Printf
import Type
import TypeEnv

type TypeofMonad = ExceptT String (State TypeofState)

data TypeofState = TypeofState
  { _nextIdentity :: Int,
    _freshCounter :: Int,
    _kindEnv :: KindEnv,
    _typeEnv :: TypeEnv
  }

initIdentity :: Int
initIdentity = 4

initKindEnv :: KindEnv
initKindEnv =
  [ ("int", (intType, Star)),
    ("sym", (symType, Star)),
    ("->", (arrowType, Arrow [Star, Star] Star)),
    ("arg", (argType, Star))
  ]

initTypeEnv :: TypeEnv
initTypeEnv = foldr (uncurry bindScheme) emptyTypeEnv schemes
  where
    schemes = map (\(name, _, scheme) -> (name, scheme)) primitives

initTypeofState :: TypeofState
initTypeofState = TypeofState 4 0 initKindEnv initTypeEnv

nextIdentity :: Lens' TypeofState Int
nextIdentity f (TypeofState i c k t) = (\i -> TypeofState i c k t) <$> f i

freshCounter :: Lens' TypeofState Int
freshCounter f (TypeofState i c k t) = (\c -> TypeofState i c k t) <$> f c

kindEnv :: Lens' TypeofState KindEnv
kindEnv f (TypeofState i c k t) = (\k -> TypeofState i c k t) <$> f k

typeEnv :: Lens' TypeofState TypeEnv
typeEnv f (TypeofState i c k t) = TypeofState i c k <$> f t

getTypeEnv :: TypeofMonad TypeEnv
getTypeEnv = gets $ view typeEnv

bindTypeEnv :: Name -> Scheme -> TypeofMonad ()
bindTypeEnv x s = modify $ typeEnv %~ bindScheme x s

bindsTypeEnv :: [Name] -> [Scheme] -> TypeofMonad ()
bindsTypeEnv xs ss = modify $ typeEnv %~ bindSchemes xs ss

localState :: Lens' TypeofState a -> (a -> a) -> TypeofMonad b -> TypeofMonad b
localState len f mv = do
  initState <- gets $ view len
  modify $ len %~ f
  v <- mv
  modify $ len .~ initState
  return v

localTypeEnv :: (TypeEnv -> TypeEnv) -> TypeofMonad a -> TypeofMonad a
localTypeEnv = localState typeEnv

getKindEnv :: TypeofMonad KindEnv
getKindEnv = gets $ view kindEnv

bindKindEnv :: Name -> (Type, Kind) -> TypeofMonad ()
bindKindEnv con (typ, kind) = modify $ kindEnv %~ bind con (typ, kind)

localKindEnv :: (KindEnv -> KindEnv) -> TypeofMonad a -> TypeofMonad a
localKindEnv = localState kindEnv

newConstructor :: String -> TypeofMonad TypeCons
newConstructor name = do
  new <- gets $ view nextIdentity
  name' <- constructorString name
  modify $ nextIdentity %~ (+ 1)
  return $ TypeCons name' new

constructorString :: Name -> TypeofMonad String
constructorString name = do
  kindEnv <- gets $ view kindEnv
  let times = repeatTimes name kindEnv
  return $
    if times == 0
      then name
      else printf "%s@{%d}" name times

txType :: TypeExp -> TypeofMonad (Type, Kind)
txType (TyCon c) = do
  kindenv <- gets $ view kindEnv
  case lookup c kindenv of
    Nothing -> throwError $ printf "TypeError: unkonwn type name %s" c
    Just (t, k) -> return (t, k)
txType (TyVar x) = do
  kindenv <- gets $ view kindEnv
  case lookup x kindenv of
    Nothing -> throwError $ printf "TypeError: unkonwn type name %s" x
    Just (t, k) -> return (t, k)
txType (ConApp tx txs) = do
  (typ, kind) <- txType tx
  (typs, kinds) <- mapM txType txs <&> unzip
  case kind of
    Arrow argks resultk ->
      if argks == kinds
        then return (TApp typ typs, resultk)
        else throwError $ printf "TypeError: expect %s but got %s" (show kind) (show (Arrow kinds resultk))
    Star -> throwError $ "TypeError: " ++ show typ ++ " is not a constructor"
txType (FunTy txs tx) = do
  (typ, kind) <- txType tx
  (typs, kinds) <- mapM txType txs <&> unzip
  if all (== Star) (kind : kinds)
    then return (funType typs typ, Star)
    else throwError "TypeError: one of types is not a type"
txType (Forall _ _) = throwError "'forall' is permissible only at top level"

txTyScheme :: TypeExp -> TypeofMonad Scheme
txTyScheme (Forall alphas tx) = do
  (typ, kind) <- localKindEnv (\e -> extend e (map (\a -> (a, (TVar a, Star))) alphas)) (txType tx)
  if kind == Star
    then return $ Scheme alphas typ
    else throwError "TypeError: expect a type but got constructor"
txTyScheme tx = do
  (typ, kind) <- txType tx
  case kind of
    Star -> return $ Scheme [] typ
    _ -> throwError "TypeError: expcet a type but got constructor"

validate :: Name -> Scheme -> Type -> Kind -> TypeofMonad ()
validate con scheme typ kind = do
  case (schemeShape scheme, kind) of
    (MonoVal typ', Star) ->
      unless
        (typ' == typ)
        (throwError $ printf "TypeError1: %s should have %s but has type %s" con desiredType (show scheme))
    (MonoFun _ result, Star) ->
      unless
        (result == typ)
        (throwError $ printf "TypeError2: %s should have %s but has type %s" con desiredType (show result))
    (PolyVal alphas typ', Arrow argkinds _) ->
      if applyTyp typ'
        then validateLength alphas argkinds >> validateTVarArgs typ'
        else throwError $ printf "TypeError3: %s should have %s but has type %s" con desiredType (show scheme)
    (PolyFun alphas _ result, Arrow argkinds _) ->
      if applyTyp result
        then validateLength alphas argkinds >> validateTVarArgs result
        else throwError $ printf "TypeError4: %s should have %s but has type %s" con desiredType (show result)
    _ -> throwError $ printf "TypeError: kind error of %s" con
  where
    applyTyp :: Type -> Bool
    applyTyp (TApp typ' _) = typ' == typ
    applyTyp _ = False
    desiredType :: String
    desiredType =
      case kind of
        Star -> printf "type %s" (show typ)
        Arrow _ _ -> printf "a type made with %s" (show typ)
    validateLength :: [Name] -> [Kind] -> TypeofMonad ()
    validateLength alphas argkinds =
      when (length alphas /= length argkinds) $ throwError (printf "TypeError: kind error of %s" con)
    asTVar (TVar a) = return a
    asTVar t = throwError $ printf "TypeError: %s is not a type variable" (show t)
    validateTVarArgs :: Type -> TypeofMonad ()
    validateTVarArgs (TApp _ typs) = do
      tvars <- mapM asTVar typs
      case duplicateName tvars of
        Nothing -> return ()
        Just a -> throwError $ printf "TypeError: duplicate %s" a
    validateTVarArgs _ = error "InternalError: impossible type arguments"

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

listType :: Type -> TypeofMonad Type
listType t = do
  kindenv <- getKindEnv
  case lookup "list" kindenv of
    Nothing -> throwError "No List type in current basis"
    Just (listCons, _) -> return $ TApp listCons [t]

unitType :: TypeofMonad Type
unitType = do
  kindenv <- getKindEnv
  case lookup "unit" kindenv of
    Nothing -> throwError "No Unit type in current basis"
    Just (unitCons, _) -> return unitCons

literalType :: Value -> TypeofMonad Type
literalType (Sym _) = return symType
literalType (Num _) = return intType
literalType (ConVal "NIL" []) = listType alpha
literalType (ConVal "CONS" [v, ConVal "NIL" []]) = literalType v >>= listType
literalType (ConVal "CONS" [v1, v2]) = do
  t1 <- literalType v1 >>= listType
  t2 <- literalType v2
  if t1 == t2
    then return t2
    else cannotUnify t1 t2
literalType _ = error "BugInTypeTypeofence: closures and primitives are not literal"

typeofDef :: Def -> TypeofMonad String
typeofDef (Val x exp) = do
  typeenv <- getTypeEnv
  (typ, cons) <- typeof exp
  s <- solve cons -- `debug` show cons
  let scheme = generalize (subst s typ) (ftvTypeEnv typeenv)
  bindTypeEnv x scheme
  return $ show scheme
typeofDef (Valrec x exp) = do
  typeenv <- getTypeEnv
  alpha <- freshType
  let alphaScheme = Scheme [] alpha
  (typ, cons) <- localTypeEnv (bindScheme x alphaScheme) (typeof exp)
  let c = Ceq alpha typ `Cand` cons
  s <- solve c
  let scheme = generalize (subst s typ) (ftvTypeEnv typeenv)
  bindTypeEnv x scheme
  return $ show scheme
typeofDef (Data name kind entries) = do
  con <- newConstructor name
  bindKindEnv name (TCon con, kind)
  let (vcons, typs) = unzip entries
  schemes <- mapM txTyScheme typs
  zipWithM_ (\n s -> validate n s (TCon con) kind) vcons schemes
  bindsTypeEnv vcons schemes
  return $ unlines (zipWith (printf "%s : %s") vcons (map show schemes))

typeof :: Exp -> TypeofMonad (Type, Constraint)
typeof (Literal v) = (,Trival) <$> literalType v
typeof (Var x) = do
  typeenv <- getTypeEnv
  case findScheme typeenv x of
    Nothing -> throwError $ "NotFound: " ++ x
    Just scheme@(Scheme names _) ->
      (,Trival) . instantiate scheme <$> freshTypes (length names)
typeof (VCon c) = do
  typeenv <- gets $ view typeEnv
  case findScheme typeenv c of
    Nothing -> throwError $ "NotFound: " ++ c
    Just scheme@(Scheme names _) ->
      (,Trival) . instantiate scheme <$> freshTypes (length names)
typeof (Lambda names exp) = do
  alphas <- freshTypes (length names)
  let schemes = map (Scheme []) alphas
  (ret, c) <- localTypeEnv (bindSchemes names schemes) (typeof exp)
  return (funType alphas ret, c)
typeof (Apply fun args) = do
  (funTyp, cFun) <- typeof fun
  (argTyps, cArgs) <- typeofMany args
  alpha <- freshType
  return (alpha, cFun `Cand` cArgs `Cand` (funTyp `Ceq` funType argTyps alpha))
typeof (Let binds body) = do
  let (names, exps) = unzip binds
  (typs, c) <- typeofMany exps
  s <- solve c
  typeenv <- getTypeEnv
  let c' = conjoin [TVar alpha `Ceq` subst s (TVar alpha) | alpha <- dom s `intersect` ftvTypeEnv typeenv]
  let schemes = [generalize (subst s typ) (ftvTypeEnv typeenv `union` ftvCons c') | typ <- typs]
  (bodyTyp, bodyCons) <- localTypeEnv (bindSchemes names schemes) (typeof body)
  return (bodyTyp, c' `Cand` bodyCons)
typeof (Letrec binds body) = do
  let (names, exps) = unzip binds
  alphas <- freshTypes (length names)
  let alphaSchemes = map (Scheme []) alphas
  (typs, cr) <- localTypeEnv (bindSchemes names alphaSchemes) (typeofMany exps)
  let c = conjoin $ cr : zipWith Ceq typs alphas
  s <- solve c
  typeenv <- getTypeEnv
  let c' = conjoin [TVar alpha `Ceq` subst s (TVar alpha) | alpha <- dom s `intersect` ftvTypeEnv typeenv]
  let schemes = [generalize (subst s typ) (ftvTypeEnv typeenv `union` ftvCons c') | typ <- typs]
  (bodyTyp, bodyCons) <- localTypeEnv (bindSchemes names schemes) (typeof body)
  return (bodyTyp, c' `Cand` bodyCons)
typeof (Case e choices) = do
  (te, ce) <- typeof e
  choicesTyps <- mapM typeofChoice choices
  let (ts, cs) = unzip choicesTyps
  alpha <- freshType
  let c' = conjoin [Ceq ti (funType [te] alpha) | ti <- ts]
  let c = ce `Cand` c' `Cand` conjoin cs
  return (alpha, c)

typeofChoice :: Choice -> TypeofMonad (Type, Constraint)
typeofChoice (p, e) = do
  (binds, t, c) <- typeofPat p
  (t', c') <- localTypeEnv (extendTypeEnv binds) (typeof e)
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
  let (envs, ts, cs) = unzip3 patsT
  alpha <- freshType
  let c = Ceq vconT (funType ts alpha)
  let c' = conjoin cs
  case disjointUnion envs of
    Left x -> throwError $ "TypeError: name " ++ x ++ " is bound multiple times in pattern"
    Right env' -> return (env', alpha, Cand c c')

typeofVCon :: VCon -> TypeofMonad Type
typeofVCon c = do
  typeenv <- gets $ view typeEnv
  case findScheme typeenv c of
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
