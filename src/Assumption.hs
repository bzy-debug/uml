module Assumption where

import Ast
import Convert
import Data.Bifunctor
import Data.List
import Scheme
import Subst

type Assumption = (Env Scheme, [Name])

emptyAssum :: Assumption
emptyAssum = (emptyEnv, [])

primitiveAssum :: Assumption
primitiveAssum = (map (\(name, _, scheme) -> (name, scheme)) primitives, [])

findScheme :: Assumption -> Name -> Maybe Scheme
findScheme (assum, _) x = lookup x assum

bindScheme :: Name -> Scheme -> Assumption -> Assumption
bindScheme x s (assum, free) =
  (bind x s assum, union free $ ftvScheme s)

bindSchemes :: [Name] -> [Scheme] -> Assumption -> Assumption
bindSchemes names schemes assum =
  foldr (uncurry bindScheme) assum (zip names schemes)

ftvAssum :: Assumption -> [Name]
ftvAssum (_, ftvs) = ftvs

substAssum :: Subst -> Assumption -> Assumption
substAssum s (assum, ftvs) =
  (map (second $ substScheme s) assum, ftvs)
