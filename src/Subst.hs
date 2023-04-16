module Subst where

import Ast
import Data.Bifunctor
import Data.List
import Type

type Subst = [(Name, Type)]

showSubst :: Subst -> String
showSubst = unwords . map (\(name, typ) -> name ++ "/" ++ show typ)

emptySubst :: Subst
emptySubst = []

singleSubst :: Name -> Type -> Subst
singleSubst x t = [(x, t)]

makeSubst :: [Name] -> [Type] -> Subst
makeSubst = zip

subst :: Subst -> Type -> Type
subst [] typ = typ
subst ss (TVar x) =
  case lookup x ss of
    Nothing -> TVar x
    Just t -> t
subst _ (TCon c) = TCon c
subst ss (TApp c ts) = TApp c (map (subst ss) ts)

compose :: Subst -> Subst -> Subst
compose s2 s1 = s2 `union` map (second (subst s2)) s1

dom :: Subst -> [Name]
dom = map fst
