module Scheme where

import Ast
import Data.List
import Subst
import Type

data Scheme = Scheme [Name] Type

type Assumption = Env Scheme

ftvScheme :: Scheme -> [Name]
ftvScheme (Scheme names typ) = ftv typ \\ names

ftvAssum :: Assumption -> [Name]
ftvAssum assum = unions (map (\(_, scheme) -> ftvScheme scheme) assum)

instantiate :: Scheme -> [Type] -> Type
instantiate (Scheme formals typ) types =
  subst (makeSubst formals types) typ

generalize :: Type -> [Name] -> Scheme
generalize typ names = Scheme (ftv typ \\ names) typ
