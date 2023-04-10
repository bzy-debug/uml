module Assumption where

import Ast
import Data.Bifunctor
import Data.List
import Scheme
import Subst

type Assumption = (Env Scheme, [Name])

emptyAssum :: Assumption
emptyAssum = (emptyEnv, [])

findScheme :: Assumption -> Name -> Maybe Scheme
findScheme (assum, _) x = lookup x assum

bindScheme :: Assumption -> Name -> Scheme -> Assumption
bindScheme (assum, free) x s =
  (bind x s assum, union free $ ftvScheme s)

ftvAssum :: Assumption -> [Name]
ftvAssum (_, ftvs) = ftvs

substAssum :: Subst -> Assumption -> Assumption
substAssum s (assum, ftvs) =
  (map (second $ substScheme s) assum, ftvs)
