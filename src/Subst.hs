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

occurs :: Name -> Type -> Bool
occurs x (TVar y) = x == y
occurs _ (TCon _) = False
occurs x (TApp _ ts) = any (occurs x) ts

varBind :: Name -> Type -> Subst
varBind x (TVar y) =
  if x == y
    then emptySubst
    else singleSubst x (TVar y)
varBind x t =
  if occurs x t
    then error "unify: occurs"
    else singleSubst x t

unify :: Type -> Type -> Subst
unify (TVar x) t = varBind x t
unify t (TVar x) = varBind x t
unify (TCon c) (TCon c') =
  if c == c'
    then emptySubst
    else error "cannot unify: diff con"
unify (TApp c ts) (TApp c' ts') =
  if c == c'
    then unifys ts ts'
    else error "cannot unify: diff cons"
  where
    unifys :: [Type] -> [Type] -> Subst
    unifys [] [] = emptySubst
    unifys (t : ts) (t' : ts') =
      let s = unify t t'
       in compose (unifys ts ts') s
    unifys _ _ = undefined
unify _ _ = error "cannot unify: im"

test :: Subst
test =
  unify
    (TApp "j" [TVar "x", TVar "y", TVar "z"])
    ( TApp
        "j"
        [ TApp "f" [TVar "y", TVar "y"],
          TApp "f" [TVar "z", TVar "z"],
          TApp "f" [TVar "a", TVar "a"]
        ]
    )
