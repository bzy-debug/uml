module Type where

import Ast
import Data.List

data Type
  = TVar Name
  | TCon Name
  | TApp Name [Type]
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

intType :: Type
intType = TCon "int"

boolType :: Type
boolType = TCon "bool"

symType :: Type
symType = TCon "sym"

funType :: [Type] -> Type -> Type
funType args ret = TApp "->" [TApp "args" args, ret]

listType :: Type -> Type
listType typ = TApp "list" [typ]

alpha :: Type
alpha = TVar "'a"

beta :: Type
beta = TVar "'b"

asFunType :: Type -> Maybe ([Type], Type)
asFunType (TApp "->" [TApp "args" args, ret]) =
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
          TCon c -> c
          TApp con types -> "(" ++ con ++ " " ++ unwords (map show types) ++ ")"
