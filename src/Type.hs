module Type where

import Basic
import Data.List

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
  | TApp TypeCons [Type]
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

intCons :: TypeCons
intCons = TypeCons "int" 0

intType :: Type
intType = TCon intCons

symCons :: TypeCons
symCons = TypeCons "sym" 1

symType :: Type
symType = TCon symCons

boolCons :: TypeCons
boolCons = TypeCons "bool" 2

boolType :: Type
boolType = TCon boolCons

arrowCons :: TypeCons
arrowCons = TypeCons "->" 3

arrowType :: Type
arrowType = TCon arrowCons

argCons :: TypeCons
argCons = TypeCons "args" 4

argType :: Type
argType = TCon argCons

funType :: [Type] -> Type -> Type
funType args ret = TApp arrowCons [TApp argCons args, ret]

listCons :: TypeCons
listCons = TypeCons "list" 5

listType :: Type -> Type
listType t = TApp listCons [t]

alpha :: Type
alpha = TVar "'a"

asFunType :: Type -> Maybe ([Type], Type)
asFunType (TApp arrow [TApp arg args, ret])
  | arrow == arrowCons && arg == argCons =
      Just (args, ret)
asFunType _ = Nothing

instance Show Type where
  show typ =
    case asFunType typ of
      Just (args, ret) ->
        "(" ++ unwords (map show args) ++ "->" ++ show ret ++ ")"
      Nothing ->
        case typ of
          TVar x -> x
          TCon c -> show c
          TApp con types -> "(" ++ show con ++ " " ++ unwords (map show types) ++ ")"
