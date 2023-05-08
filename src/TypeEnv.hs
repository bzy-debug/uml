module TypeEnv where

import Basic
import Data.Bifunctor
import Data.List
import Primitives
import Scheme
import Subst

type TypeEnv = (Env Scheme, [Name])

emptyTypeEnv :: TypeEnv
emptyTypeEnv = (emptyEnv, [])

primitiveTypeEnv :: TypeEnv
primitiveTypeEnv = (map (\(name, _, scheme) -> (name, scheme)) primitives, [])

findScheme :: TypeEnv -> Name -> Maybe Scheme
findScheme (typeEnv, _) x = lookup x typeEnv

bindScheme :: Name -> Scheme -> TypeEnv -> TypeEnv
bindScheme x s (typeEnv, free) =
  (bind x s typeEnv, union free $ ftvScheme s)

bindSchemes :: [Name] -> [Scheme] -> TypeEnv -> TypeEnv
bindSchemes names schemes typeEnv =
  foldr (uncurry bindScheme) typeEnv (zip names schemes)

extendTypeEnv :: Env Scheme -> TypeEnv -> TypeEnv
extendTypeEnv binds typeEnv = foldr (uncurry bindScheme) typeEnv binds

ftvTypeEnv :: TypeEnv -> [Name]
ftvTypeEnv (_, ftvs) = ftvs

substTypeEnv :: Subst -> TypeEnv -> TypeEnv
substTypeEnv s (typeEnv, ftvs) =
  (map (second $ substScheme s) typeEnv, ftvs)
