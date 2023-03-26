{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypedAst where

import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Except
import qualified Data.Set as Set

type Name = String

type Env a = [(Name, a)]

newtype TypeError = TypeError {getError :: String}

instance Show TypeError where
  show (TypeError e) = "TypeError: " ++ e

instance Exception TypeError

data Kind
  = Type
  | Arrow [Kind] Kind
  deriving (Eq, Show)

data TExpr
  = TCons Name
  | TApp TExpr [TExpr]
  | TFun [TExpr] TExpr
  | TForall [Name] TExpr
  | TVar Name
  deriving (Show)

data Value
  = VSym Name
  | VNum Int
  | VBool Bool
  | VNil
  | VPair Value Value
  | VClosure [Name] Expr (Env Value)

data Expr
  = Literal Value
  | Var Name
  | If Expr Expr Expr
  | While Expr Expr
  | Begin [Expr]
  | Apply Expr [Expr]
  | Let [(Name, Expr)] Expr
  | Letrec [(Name, TExpr, Expr)] Expr
  | Lambda [(Name, TExpr)] Expr
  | TLambda [Name] Expr
  | TApply Expr [TExpr]

data Define
  = Val Name Expr
  | Valrec Name TExpr Expr
  | Expr Expr
  | Define Name TExpr [(Name, TExpr)] Expr

nats :: [Int]
nats = [1, 2 ..]

infiniteTyVars :: [Name]
infiniteTyVars = map (\i -> "'b-" ++ show i) nats

delta0 :: Env Kind
delta0 =
  [ ("int", Type),
    ("bool", Type),
    ("sym", Type),
    ("unit", Type),
    ("pair", Arrow [Type, Type] Type),
    ("sum", Arrow [Type, Type] Type),
    ("list", Arrow [Type] Type)
  ]

kindof :: TExpr -> Env Kind -> Either TypeError Kind
kindof tau delta =
  kind tau
  where
    kind :: TExpr -> Either TypeError Kind
    kind (TVar v) = do
      case lookup v delta of
        Nothing -> throwError $ TypeError ("unknown type variable " ++ v)
        Just k -> return k
    kind (TCons c) = do
      case lookup c delta of
        Nothing -> throwError $ TypeError ("unknown type constructor " ++ c)
        Just k -> return k
    kind (TFun args result) = do
      resultKind <- kind result
      if resultKind /= Type
        then throwError $ TypeError "function result is not a type"
        else do
          argKinds <- mapM kind args
          if any (/= Type) argKinds
            then throwError $ TypeError "argument list includes a non-type"
            else return Type
    kind (TApp tau actuals) = do
      tauKind <- kind tau
      case tauKind of
        Type -> throwError $ TypeError ("tried to apply type " ++ show tau ++ " as type constructor")
        Arrow formalKinds resultKind -> do
          actualKinds <- mapM kind actuals
          if and $ zipWith (==) formalKinds actualKinds
            then return resultKind
            else throwError $ TypeError ("type constructor" ++ show tau ++ " applied to the wrong arguments")
    kind (TForall vars tau) = do
      let delta' = zip vars (replicate (length vars) Type) ++ delta
      tauKind <- kindof tau delta'
      case tauKind of
        Type -> return Type
        Arrow {} -> throwError $ TypeError "quantifed a non‐nullary type constructor"

asList :: TExpr -> Either TypeError TExpr
asList = undefined

asFun :: TExpr -> Either TypeError ([TExpr], TExpr)
asFun = undefined

asForall :: TExpr -> Either TypeError ([Name], TExpr)
asForall = undefined

freetyvars :: TExpr -> Set.Set Name
freetyvars (TCons _) = Set.empty
freetyvars (TVar x) = Set.singleton x
freetyvars (TApp ty tys) = Set.unions $ map freetyvars (ty : tys)
freetyvars (TFun tys ty) = Set.unions $ map freetyvars (ty : tys)
freetyvars (TForall vars ty) = freetyvars ty `Set.difference` Set.fromList vars

freetyvarsGamma :: Env TExpr -> Set.Set Name
freetyvarsGamma = foldl (\ftvs (_, ty) -> freetyvars ty `Set.union` ftvs) Set.empty

rename :: [Name] -> [Name] -> TExpr -> TExpr
rename = undefined

eqType :: TExpr -> TExpr -> Bool
eqType (TVar a) (TVar a') = a == a'
eqType (TCons a) (TCons a') = a == a'
eqType (TApp t ts) (TApp t' ts') = eqType t t' && eqTypes ts ts'
eqType (TFun ts t) (TFun ts' t') = eqTypes ts ts' && eqType t t'
eqType (TForall vars t) (TForall vars' t') =
  let ok a = (a `Set.notMember` freetyvars t) && (a `Set.notMember` freetyvars t')
      betas = take (length vars) (filter ok infiniteTyVars)
   in length vars == length vars' && eqType (rename vars betas t) (rename vars' betas t')
eqType _ _ = False

eqTypes :: [TExpr] -> [TExpr] -> Bool
eqTypes tl1 tl2 = and (zipWith eqType tl1 tl2)

assertEqType :: TExpr -> TExpr -> Either TypeError ()
assertEqType = undefined

assertGoodType :: Env Kind -> TExpr -> Either TypeError ()
assertGoodType = undefined

substitute :: TExpr -> [Name] -> [TExpr] -> TExpr
substitute = undefined

typeof :: Expr -> Env Kind -> Env TExpr -> Either TypeError TExpr
typeof expr delta gamma = typ expr
  where
    typ :: Expr -> Either TypeError TExpr
    typ (Literal val) =
      case val of
        VSym _ -> return $ TCons "sym"
        VNum _ -> return $ TCons "int"
        VBool _ -> return $ TCons "bool"
        VNil -> return $ TForall ["a"] (TApp (TCons "list") [TVar "a"])
        VPair v VNil -> do
          t <- typ (Literal v)
          return $ TApp (TCons "list") [t]
        VPair v v' -> do
          t <- typ (Literal v)
          t' <- typ (Literal v')
          l <- asList t'
          assertEqType t l
          return (TApp (TCons "list") [t])
        VClosure {} -> throwError $ TypeError "unreach"
    typ (Var x) =
      -- maybe (throwError $ TypeError "variable not found") return (Map.lookup x gamma)
      case lookup x gamma of
        Nothing -> throwError $ TypeError "variable not found"
        Just t -> return t
    typ (While cond body) = do
      condt <- typ cond
      assertEqType condt (TCons "bool")
      _ <- typ body
      return $ TCons "unit"
    typ (If cond ifso ifelse) = do
      condt <- typ cond
      assertEqType condt (TCons "bool")
      sot <- typ ifso
      elset <- typ ifelse
      assertEqType sot elset
      return sot
    typ (Begin exprs) = iter exprs (TCons "unit")
      where
        iter [] lastTyp = return lastTyp
        iter (e : es) _ = typ e >>= iter es
    typ (Let binds body) = do
      let (names, exprs) = unzip binds
      types <- mapM typ exprs
      typeof body delta (zip names types ++ gamma)
    typ (Letrec binds body) = do
      let (names, types, exprs) = unzip3 binds
      forM_ types (assertGoodType delta)
      let gamma' = zip names types ++ gamma
      forM_ exprs (\e -> typeof e delta gamma')
      typeof body delta gamma'
    typ (Lambda nameTypes body) = do
      let (_, types) = unzip nameTypes
      forM_ types (assertGoodType delta)
      retType <- typeof body delta (nameTypes ++ gamma)
      return $ TFun types retType
    typ (Apply fun args) = do
      (expects, ret) <- typ fun >>= asFun
      argTypes <- mapM typ args
      zipWithM_ assertEqType expects argTypes
      return ret
    typ (TApply expr types) = do
      (names, body) <- typ expr >>= asForall
      forM_ types (assertGoodType delta)
      return $ substitute body names types
    typ (TLambda names body) = do
      forM_ names (ok gamma)
      typeof body (zip names (replicate (length names) Type)) gamma
      where
        ok :: Env TExpr -> Name -> Either TypeError ()
        ok gamma name =
          if name `Set.notMember` freetyvarsGamma gamma
            then return ()
            else throwError $ TypeError "sdg"
