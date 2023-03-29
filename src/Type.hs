{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Type where

import Ast
import Data.List
import Data.Maybe

type TyVar = Name

type TyCon = Name

type Subst = Env Type

data Type
  = TVar TyVar
  | TCon TyCon
  | ConApp Type [Type]
  deriving (Eq)

data TypScheme = Forall [Name] Type

dom :: Subst -> [Name]
dom theta = nub $ map fst theta

varsubst :: Subst -> (Name -> Type)
varsubst theta =
  \a -> fromMaybe (TVar a) (lookup a theta)

tysubst :: Subst -> Type -> Type
tysubst theta = subst
  where
    subst :: Type -> Type
    subst (TVar a) = varsubst theta a
    subst (TCon c) = TCon c
    subst (ConApp tau taus) = ConApp (subst tau) (map subst taus)

compose :: Subst -> Subst -> Subst
compose theta1 theta2 =
  let domain = dom theta1 `union` dom theta2
      replace = tysubst theta1 . varsubst theta2
   in zip domain (map replace domain)

(|-->) :: Name -> Type -> Subst
a |--> (TVar a') = [(a, TVar a') | a /= a']
a |--> tau = [(a, tau)]

freetyvars :: Type -> [Name]
freetyvars (TVar a) = [a]
freetyvars (TCon _) = []
freetyvars (ConApp ty tys) = (nub . concat) $ freetyvars ty : map freetyvars tys

intType :: Type
intType = TCon "int"

boolType :: Type
boolType = TCon "bool"

symType :: Type
symType = TCon "sym"

alpha :: Type
alpha = TVar "a"

beta :: Type
beta = TVar "b"

unitType :: Type
unitType = TCon "unit"

listType :: Type -> Type
listType typ = ConApp (TCon "list") [typ]

pairType :: Type -> Type -> Type
pairType x y = ConApp (TCon "pair") [x, y]

funType :: [Type] -> Type -> Type
funType args result = ConApp (TCon "function") [ConApp (TCon "arguments") args, result]

asFuntype :: Type -> Maybe ([Type], Type)
asFuntype
  ( ConApp
      (TCon "function")
      [ConApp (TCon "arguments") args, result]
    ) = Just (args, result)
asFuntype _ = Nothing

instance Show Type where
  show tau =
    case asFuntype tau of
      Just (args, result) ->
        "(" ++ unwords (map show args) ++ " -> " ++ show result ++ ")"
      Nothing ->
        case tau of
          TCon c -> c
          TVar a -> a
          ConApp tau [] -> "(" ++ show tau ++ ")"
          ConApp tau taus ->
            "("
              ++ show tau
              ++ " "
              ++ unwords (map show taus)
              ++ ")"

instance Show TypScheme where
  show (Forall [] tau) = show tau
  show (Forall as tau) =
    "(∀ [" ++ unwords as ++ "] " ++ show tau ++ ")"
