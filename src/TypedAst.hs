module TypedAst where

import Data.Map as Map
import Control.Exception (Exception)
import Control.Monad.Except

type Name = String

type Env a = Map.Map Name a

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

delta0 :: Env Kind
delta0 =
  Map.fromList
    [ ("int", Type),
      ("bool", Type),
      ("char", Type),
      ("pair", Arrow [Type, Type] Type),
      ("sum", Arrow [Type, Type] Type),
      ("list", Arrow [Type] Type)
    ]


typeof :: Expr -> Env Kind -> Env TExpr -> TExpr
typeof = undefined

kindof :: TExpr -> Env Kind -> Either TypeError Kind
kindof tau delta =
  kind tau
  where
    kind :: TExpr -> Either TypeError Kind
    kind (TVar v) = do
      case Map.lookup v delta of
        Nothing -> throwError $ TypeError ("unknown type variable " ++ v)
        Just k -> return k
    kind (TCons c) = do
      case Map.lookup c delta of
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
    kind (TApp tau' actuals) = do
      tauKind <- kind tau'
      case tauKind of
        Type -> throwError $ TypeError ("tried to apply type " ++ show tau ++ "as type constructor")
        Arrow formalKinds resultKind -> do
          actualKinds <- mapM kind actuals
          if and $ zipWith (==) formalKinds actualKinds
            then return resultKind
            else throwError $ TypeError ("type constructor" ++ show tau ++ " applied to the wrong arguments")
    kind (TForall vars tau') = do
      let addons = Map.fromList $ zip vars (replicate (length vars) Type)
      let delta' = Map.union delta addons
      tauKind <- kindof tau' delta'
      case tauKind of
        Type -> return Type
        Arrow {} -> throwError $ TypeError "quantifed a non‐nullary type constructor"
