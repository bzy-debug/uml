{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Mono where

import Ast
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)

isList :: Value -> Bool
isList Nil = True
isList (Pair _ v') = isList v'
isList _ = False

type TyVar = Name

type TyCon = Name

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

dom :: Subst -> [Name]
dom theta = nub $ map fst theta

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
  let domain = dom theta1 `union` dom theta2
      replace = tysubst theta1 . varsubst theta2
   in zip domain (map replace domain)

instantiate :: TypScheme -> [Typ] -> Typ
instantiate (Forall formals tau) actuals =
  if length formals == length actuals
    then tysubst (zip formals actuals) tau
    else error "Internal error"

(|-->) :: Name -> Typ -> Subst
a |--> (TVar a') = [(a, TVar a') | a /= a']
a |--> tau = [(a, tau)]

freetyvars :: Typ -> [Name]
freetyvars (TVar a) = [a]
freetyvars (TCon _) = []
freetyvars (ConApp ty tys) = (nub . concat) $ freetyvars ty : map freetyvars tys

canonicalize :: TypScheme -> TypScheme
canonicalize (Forall bound ty) =
  let canonicalTyvarName n =
        if n < 26
          then ['\'', chr (ord 'a' + n)]
          else "'v" ++ show (n - 25)
      free = freetyvars ty \\ bound
      unusedIndex n =
        if canonicalTyvarName n `elem` free
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

generalize :: Typ -> [Name] -> TypScheme
generalize tau tyvars =
  canonicalize
    ( Forall
        ( freetyvars tau \\ tyvars
        )
        tau
    )

freshInstance :: TypScheme -> Typ
freshInstance t@(Forall bound _) =
  instantiate t (take (length bound) freshtyvar)

type TypeEnv = (Env TypScheme, [Name])

emptyTypeEnv :: TypeEnv
emptyTypeEnv = ([], [])

findtyscheme :: Name -> TypeEnv -> TypScheme
findtyscheme x (gamma, _) =
  fromMaybe (error "not found") (lookup x gamma)

bindtyscheme :: (Name, TypScheme) -> TypeEnv -> TypeEnv
bindtyscheme (x, sigma@(Forall bound tau)) (gamma, free) =
  ( (x, sigma) : gamma,
    (freetyvars tau \\ bound) `union` free
  )

freetyvarsGamma :: TypeEnv -> [Name]
freetyvarsGamma (_, free) = free

infix 4 :~

infixl 3 :/\

data Con
  = (:~) Typ Typ
  | (:/\) Con Con
  | Trivial

freetyvarsCon :: Con -> [Name]
freetyvarsCon (t :~ t') = freetyvars t `union` freetyvars t'
freetyvarsCon (c :/\ c') = freetyvarsCon c `union` freetyvarsCon c'
freetyvarsCon Trivial = []

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
      if x `elem` freetyvars t2
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
      ty (Begin es) =
        let go [] tau = tau
            go (e : es) _ = go es (fst (ty e))
         in (go es unitType, Trivial)
      ty (Lambda names body) =
        let alphas = take (length names) freshtyvar
            schemes = map (Forall []) alphas
            gamma' = foldr bindtyscheme gamma (zip names schemes)
            (tau, c) = typeof body gamma'
         in (funType alphas tau, c)
      ty (Letx Let bs body) =
        let (xs, es) = unzip bs
            (ts, c) = typesof es gamma
            theta = solve c
            c' = conjoinCons [TVar a :~ tysubst theta (TVar a) | a <- dom theta `intersect` freetyvarsGamma gamma]
            schemes = [generalize (tysubst theta t) (freetyvarsGamma gamma `union` freetyvarsCon c') | t <- ts]
            (tau, cb) = typeof body (foldr bindtyscheme gamma (zip xs schemes))
         in (tau, cb :/\ c')
      ty (Letx LetRec bs body) =
        let (xs, es) = unzip bs
            alphas = take (length xs) freshtyvar
            schemes = map (Forall []) alphas
            gamma' = foldr bindtyscheme gamma (zip xs schemes)
            (taus, cr) = typesof es gamma'
            c = conjoinCons (cr : zipWith (:~) taus alphas)
            theta = solve c
            c' = conjoinCons [TVar a :~ tysubst theta (TVar a) | a <- dom theta `intersect` freetyvarsGamma gamma]
            schemes' = [generalize tau (freetyvarsGamma gamma `union` freetyvarsCon c') | tau <- taus]
            (tau, cb) = typeof body (foldr bindtyscheme gamma (zip xs schemes'))
         in (tau, c' :/\ cb)
   in ty e
