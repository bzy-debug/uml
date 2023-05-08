module Basic where

data Sexp
  = Atom String
  | Slist [Sexp]

instance Show Sexp where
  show (Atom s) = s
  show (Slist ss) = "(" ++ unwords (map show ss) ++ ")"

type Name = String

type Ref = Int

type VCon = Name

type Env a = [(Name, a)]

emptyEnv :: Env a
emptyEnv = []

bind :: Name -> a -> Env a -> Env a
bind x v env = (x, v) : env

binds :: [Name] -> [a] -> Env a -> Env a
binds names vals env = zip names vals ++ env

extend :: Env a -> Env a -> Env a
extend e1 e2 = e2 ++ e1

repeatTimes :: String -> Env a -> Int
repeatTimes _ [] = 0
repeatTimes name ((name', _):es) =
  if name == name' then 1 + repeatTimes name es else repeatTimes name es

disjointUnion :: [Env a] -> Either String (Env a)
disjointUnion envs =
  let env = concat envs
   in case duplicateName (map fst env) of
        Nothing -> Right env
        Just x -> Left x

duplicateName :: [Name] -> Maybe Name
duplicateName [] = Nothing
duplicateName (x : xs) =
  if x `elem` xs
    then Just x
    else duplicateName xs
