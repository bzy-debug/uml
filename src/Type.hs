module Type where

import Basic
import Data.List
import Text.Printf

data TypeExp
  = TyCon Name
  | ConApp TypeExp [TypeExp]
  | FunTy [TypeExp] TypeExp
  | Forall [Name] TypeExp
  | TyVar Name
  deriving Show

type TypeId = Int

data TypeCons = TypeCons
  { printName :: String,
    id :: TypeId
  }

instance Eq TypeCons where
  (==) (TypeCons _ i1) (TypeCons _ i2) = i1 == i2

instance Show TypeCons where
  show (TypeCons name _) = name

data Type
  = TVar Name
  | TCon TypeCons
  | TApp Type [Type]
  deriving (Eq)

unions :: Eq a => [[a]] -> [a]
unions = foldr union []

ftv :: Type -> [Name]
ftv (TVar x) = [x]
ftv (TCon _) = []
ftv (TApp _ types) = unions (map ftv types)

occurs :: Name -> Type -> Bool
occurs x (TVar y) = x == y
occurs _ (TCon _) = False
occurs x (TApp _ ts) = any (occurs x) ts

asFunType :: Type -> Maybe ([Type], Type)
asFunType
  ( TApp
      (TCon (TypeCons _ 2))
      [TApp (TCon (TypeCons _ 3)) args, ret]
    ) =
    Just (args, ret)
asFunType _ = Nothing

instance Show Type where
  show typ =
    case asFunType typ of
      Just (args, ret) ->
        printf "(-> (%s) %s)" (unwords (map show args)) (show ret)
      Nothing ->
        case typ of
          TVar x -> x
          TCon c -> show c
          TApp con types ->
            printf "(%s %s)" (show con) (unwords (map show types))
