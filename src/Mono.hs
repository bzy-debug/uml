{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Mono where

import Control.Monad.State
import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

type Name = String

type TyVar = Name

type TyCon = Name

type Env a = [(Name, a)]

type Ref = Int

data RefState = RefState
  { mem :: [(Ref, Env Value)],
    ref :: Ref
  }

type EvalMonad = StateT RefState (Either String)

newRef :: Env Value -> EvalMonad Ref
newRef env = do
  RefState {mem = mem, ref = ref} <- get
  put $
    RefState
      { mem = (ref, env) : mem,
        ref = ref + 1
      }
  return $ ref + 1

data Value
  = Sym Name
  | Num Int
  | Bool Bool
  | Nil
  | Pair Value Value
  | Closure [Name] Exp Ref
  | Primitive

data LetFlavor = Let | LetRec | LetStar

data Exp
  = Literal Value
  | Var Name
  | If Exp Exp Exp
  | Begin [Exp]
  | Apply Exp [Exp]
  | Letx LetFlavor [(Name, Exp)] Exp
  | Lambda [Name] Exp

data Def
  = Val Name Exp
  | ValRec Name Exp
  | Exp Exp
  | Define Name [Name] Exp

eval :: Exp -> Env Value -> EvalMonad Value
eval = undefined

data Typ
  = TVar TyVar
  | TCon TyCon
  | ConApp Typ [Typ]
  deriving (Eq, Show)

intType :: Typ
intType = TCon "int"

boolType :: Typ
boolType = TCon "bool"

symType :: Typ
symType = TCon "sym"

alpha :: Typ
alpha = TVar "a"

beta :: Typ
beta = TVar "b"

unitType :: Typ
unitType = TCon "unit"

listType :: Typ -> Typ
listType typ = ConApp (TCon "list") [typ]

pairType :: Typ -> Typ -> Typ
pairType x y = ConApp (TCon "pair") [x, y]

funType :: [Typ] -> Typ -> Typ
funType args result = ConApp (TCon "function") [ConApp (TCon "arguments") args, result]

asFuntype :: Typ -> Maybe ([Typ], Typ)
asFuntype
  ( ConApp
      (TCon "function")
      [ConApp (TCon "arguments") args, result]
    ) = Just (args, result)
asFuntype _ = Nothing

data TypScheme = Forall [Name] Typ deriving (Show)

type Subst = Env Typ

dom :: Subst -> Set.Set Name
dom theta = Set.fromList $ map fst theta

varsubst :: Subst -> (Name -> Typ)
varsubst theta =
  \a -> fromMaybe (TVar a) (lookup a theta)

tysubst :: Subst -> Typ -> Typ
tysubst theta = subst
  where
    subst :: Typ -> Typ
    subst (TVar a) = varsubst theta a
    subst (TCon c) = TCon c
    subst (ConApp tau taus) = ConApp (subst tau) (map subst taus)

compose :: Subst -> Subst -> Subst
compose theta1 theta2 =
  let domain = Set.toList $ Set.union (dom theta1) (dom theta2)
      replace = tysubst theta1 . varsubst theta2
   in zip domain (map replace domain)

instantiate :: TypScheme -> [Typ] -> Typ
instantiate (Forall formals tau) actuals =
  if length formals == length actuals
    then tysubst (zip formals actuals) tau
    else error "Internal error"

-- TODO Idempotence
(|-->) :: Name -> Typ -> Subst
a |--> (TVar a') = [(a, TVar a') | a /= a']
a |--> tau = [(a, tau)]

freetyvars :: Typ -> Set.Set Name
freetyvars (TVar a) = Set.singleton a
freetyvars (TCon _) = Set.empty
freetyvars (ConApp ty tys) = Set.unions $ freetyvars ty : map freetyvars tys

canonicalize :: TypScheme -> TypScheme
canonicalize (Forall bound ty) =
  let canonicalTyvarName n =
        if n < 26
          then ['\'', chr (ord 'a' + n)]
          else "'v" ++ show (n - 25)
      free = Set.difference (freetyvars ty) (Set.fromList bound)
      unusedIndex n =
        if Set.member (canonicalTyvarName n) free
          then unusedIndex (n + 1)
          else n
      newBoundVars _ [] = []
      newBoundVars index (_ : oldvars) =
        let n = unusedIndex index
         in canonicalTyvarName n : newBoundVars (n + 1) oldvars
      newBound = newBoundVars 0 bound
   in Forall newBound (tysubst (zip bound (map TVar newBound)) ty)

nats :: [Int]
nats = 1 : map (+ 1) nats

freshtyvar :: [Typ]
freshtyvar = map (\i -> TVar $ "'t" ++ show i) nats

generalize :: Typ -> Set.Set Name -> TypScheme
generalize tau tyvars =
  canonicalize
    ( Forall
        ( Set.toList $ Set.difference (freetyvars tau) tyvars
        )
        tau
    )

freshInstantiate :: TypScheme -> Typ
freshInstantiate t@(Forall bound _) =
  instantiate t (take (length bound) freshtyvar)
