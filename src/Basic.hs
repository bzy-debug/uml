module Basic where

type Name = String

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
