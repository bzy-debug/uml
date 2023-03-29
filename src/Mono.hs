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

isList :: Value -> Bool
isList Nil = True
isList (Pair _ v') = isList v'
isList _ = False

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

tyvars :: [Typ]
tyvars = map (\i -> TVar $ "'t" ++ show i) nats

freshtyvar :: [Typ]
freshtyvar = undefined

generalize :: Typ -> Set.Set Name -> TypScheme
generalize tau tyvars =
  canonicalize
    ( Forall
        ( Set.toList $ Set.difference (freetyvars tau) tyvars
        )
        tau
    )

freshInstance :: TypScheme -> Typ
freshInstance t@(Forall bound _) =
  instantiate t (take (length bound) freshtyvar)

type TypeEnv = (Env TypScheme, Set.Set Name)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = ([], Set.empty)

findtyscheme :: Name -> TypeEnv -> TypScheme
findtyscheme x (gamma, _) =
  fromMaybe (error "not found") (lookup x gamma)

bindtyscheme :: (Name, TypScheme) -> TypeEnv -> TypeEnv
bindtyscheme (x, sigma@(Forall bound tau)) (gamma, free) =
  ( (x, sigma) : gamma,
    Set.union (Set.difference (freetyvars tau) (Set.fromList bound)) free
  )

freetyvarsGamma :: TypeEnv -> Set.Set Name
freetyvarsGamma (_, free) = free

infix 4 :~

infixl 3 :/\

data Con
  = (:~) Typ Typ
  | (:/\) Con Con
  | Trivial

freetyvarsCon :: Con -> Set.Set Name
freetyvarsCon (t :~ t') = Set.union (freetyvars t) (freetyvars t')
freetyvarsCon (c :/\ c') = Set.union (freetyvarsCon c) (freetyvarsCon c')
freetyvarsCon Trivial = Set.empty

consubst :: Subst -> Con -> Con
consubst theta (tau1 :~ tau2) = tysubst theta tau1 :~ tysubst theta tau2
consubst theta (c1 :/\ c2) = consubst theta c1 :/\ consubst theta c2
consubst _ Trivial = Trivial

conjoinCons :: [Con] -> Con
conjoinCons = foldr (:/\) Trivial

-- conjoinCons [] = Trivial
-- conjoinCons [c] = c
-- conjoinCons (c : cs) = c :/\ conjoinCons cs

unstaisfiableEq :: Typ -> Typ -> a
unstaisfiableEq = undefined

solve :: Con -> Subst
solve Trivial = []
solve (c1 :/\ c2) =
  let s1 = solve c1
      s2 = solve $ consubst s1 c2
   in compose s1 s2
solve (t1 :~ t2) =
  case (t1, t2) of
    (TVar x, t2) ->
      if x `Set.member` freetyvars t2
        then case t2 of
          TVar y | x == y -> []
          _ -> error "cannot unify"
        else x |--> t2
    (t1, TVar _) -> solve (t2 :~ t1)
    (TCon c, TCon c') ->
      if c == c' then [] else error "cannot unify"
    (ConApp t ts, ConApp t' ts') ->
      let con = conjoinCons $ (t :~ t') : zipWith (:~) ts ts'
       in solve con
    _ -> error "cannot unify"

isSolved :: Con -> Bool
isSolved Trivial = True
isSolved (t :~ t') = t == t'
isSolved (c :/\ c') = isSolved c && isSolved c'

solves :: Subst -> Con -> Bool
solves theta c = isSolved $ consubst theta c

typeof :: Exp -> TypeEnv -> (Typ, Con)
typeof e gamma =
  let typesof :: [Exp] -> TypeEnv -> ([Typ], Con)
      typesof [] _ = ([], Trivial)
      typesof (e : es) gamma =
        let (tau, c) = typeof e gamma
            (taus, c') = typesof es gamma
         in (tau : taus, c :/\ c')
      literal :: Value -> Typ
      literal (Sym _) = symType
      literal (Num _) = intType
      literal (Bool _) = boolType
      literal Nil = listType alpha
      literal val@(Pair v v')
        | isList val =
            let t = literal v
             in case v' of
                  Nil -> listType t
                  _ ->
                    if literal v' == listType t
                      then listType t
                      else error "list type not consist"
      literal (Pair v v') = pairType (literal v) (literal v')
      literal _ = error "undefined"
      ty :: Exp -> (Typ, Con)
      ty (Literal v) = (literal v, Trivial)
      ty (Var x) = (freshInstance (findtyscheme x gamma), Trivial)
      ty (Apply f actuals) =
        case typesof (f : actuals) gamma of
          ([], _) -> error "pattern match"
          (funty : actualtypes, c) ->
            let rettype = head freshtyvar
             in (rettype, c :/\ (funty :~ funType actualtypes rettype))
      ty (Letx LetStar [] body) = ty body
      ty (Letx LetStar (b : bs) body) =
        ty (Letx Let [b] (Letx LetStar bs body))
      ty (If e1 e2 e3) =
        let (t1, c1) = ty e1
            (t2, c2) = ty e2
            (t3, c3) = ty e3
         in (t2, c1 :/\ c2 :/\ c3 :/\ t1 :~ boolType :/\ t2 :~ t3)
      ty (Begin _) = undefined
      ty (Lambda _ _) = undefined
      ty (Letx Let bs body) =
        let (xs, es) = unzip bs
            (ts, c) = typesof es gamma
            theta = solve c
            c' = conjoinCons [TVar a :~ tysubst theta (TVar a) | a <- Set.toList $ Set.intersection (dom theta) (freetyvarsGamma gamma)]
            schemes = [generalize (tysubst theta t) (Set.union (freetyvarsGamma gamma) (freetyvarsCon c')) | t <- ts]
            (tau, cb) = typeof body (foldr bindtyscheme gamma (zip xs schemes))
         in (tau, cb :/\ c')
      ty (Letx LetRec bs body) = undefined
   in ty e
