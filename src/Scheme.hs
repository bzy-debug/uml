module Scheme where

import Data.Char
import Data.List
import Basic
import Subst
import Type

data Scheme = Scheme [Name] Type

instance Show Scheme where
  show (Scheme [] typ) = show typ
  show (Scheme names typ) =
    "(forall [" ++ unwords names ++ "] " ++ show typ ++ ")"

ftvScheme :: Scheme -> [Name]
ftvScheme (Scheme names typ) = ftv typ \\ names

substScheme :: Subst -> Scheme -> Scheme
substScheme s (Scheme names typ) = Scheme names (subst s typ)

instantiate :: Scheme -> [Type] -> Type
instantiate (Scheme formals typ) types =
  subst (makeSubst formals types) typ

generalize :: Type -> [Name] -> Scheme
generalize typ names = canonicalize $ Scheme (ftv typ \\ names) typ

canonicalize :: Scheme -> Scheme
canonicalize (Scheme bound typ) =
  Scheme newBound (subst (makeSubst bound (map TVar newBound)) typ)
  where
    newBound = newBoundVars 0 bound
    newBoundVars _ [] = []
    newBoundVars index (_ : oldvars) =
      let n = unusedIndex index
       in canonicalTyvarName n : newBoundVars (n + 1) oldvars
    unusedIndex n =
      if canonicalTyvarName n `elem` free
        then unusedIndex n + 1
        else n
    free = ftv typ \\ bound
    canonicalTyvarName n =
      if n < 26
        then ['\'', chr $ ord 'a' + n]
        else "'v" ++ show (n - 25)
