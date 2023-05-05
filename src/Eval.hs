module Eval where

import Core
import Basic
import Control.Monad.Except
import Control.Monad.State
import Lens.Micro
import Lens.Micro.Extras

data EnvRef = EnvRef
  { _mem :: [(Ref, Env Value)],
    _ref :: Ref
  }

data EvalState = EvalState
  { _valueEnv :: Env Value,
    _envRef :: EnvRef
  }

valueEnv :: Lens' EvalState (Env Value)
valueEnv f (EvalState e r) = (`EvalState` r) <$> f e

envRef :: Lens' EvalState EnvRef
envRef f (EvalState e r) = EvalState e <$> f r

mem :: Lens' EnvRef [(Ref, Env Value)]
mem f (EnvRef m r) = (`EnvRef` r) <$> f m

ref :: Lens' EnvRef Ref
ref f (EnvRef m r) = EnvRef m <$> f r

type EvalMonad = ExceptT String (State EvalState)

newRef :: Env Value -> EvalMonad Ref
newRef env = do
  evalState <- get
  let ref' = evalState ^. (envRef . ref)
  put $ evalState & (envRef . mem) %~ (\m -> (ref', env) : m) & (envRef . ref) %~ (+ 1)
  return ref'

writeRef :: Ref -> Env Value -> EvalMonad ()
writeRef r e = modify $ (envRef . mem) %~ (\m -> (r, e) : m)

readRef :: Ref -> EvalMonad (Env Value)
readRef r = do
  mem' <- gets $ view (envRef . mem)
  case lookup r mem' of
    Nothing -> throwError $ "NotFound: " ++ show r
    Just env -> return env

lookupEnv :: Name -> EvalMonad Value
lookupEnv x = do
  env <- gets $ view valueEnv
  case lookup x env of
    Nothing -> throwError $ "NotFound: " ++ x
    Just v -> return v

bindValue :: Name -> Value -> EvalMonad ()
bindValue x v = modify $ valueEnv %~ bind x v

bindValues :: [Name] -> [Value] -> EvalMonad ()
bindValues xs vs = modify $ valueEnv %~ binds xs vs

replaceEnv :: Env Value -> EvalMonad ()
replaceEnv env = modify $ valueEnv .~ env

extendEnv :: Env Value -> EvalMonad ()
extendEnv env = modify $ valueEnv %~ (`extend` env)

getEnv :: EvalMonad (Env Value)
getEnv = gets $ view valueEnv

asLambda :: Exp -> ([Name], Exp)
asLambda (Lambda formals body) = (formals, body)
asLambda _ = error "BugInParser: rhs of letrec should be lambda"

evalDef :: Def -> EvalMonad String
evalDef (Val x e) = do
  v <- evalExp e
  bindValue x v
  return $ x ++ " = " ++ show v
evalDef (Valrec x e) = do
  env <- getEnv
  envRef <- newRef env
  let closure = Closure (asLambda e) envRef
  let newEnv = bind x closure env
  writeRef envRef newEnv
  replaceEnv newEnv
  return $ show x ++ " = <closure>"
evalDef (Data name _ _) = return name

evalExp :: Exp -> EvalMonad Value
evalExp (Literal v) = return v
evalExp (Var x) = lookupEnv x
evalExp (VCon x) = lookupEnv x
evalExp (Begin exps) =
  let iter [] lastVal = lastVal
      iter (e : es) _ = iter es (evalExp e)
   in iter exps undefined
evalExp (Apply fun args) = do
  funVal <- evalExp fun
  case funVal of
    Primitive prim -> do
      actuals <- mapM evalExp args
      return $ prim actuals
    Closure (formals, body) ref -> do
      savedEnv <- readRef ref
      actuals <- mapM evalExp args
      replaceEnv (binds formals actuals savedEnv)
      evalExp body
    _ -> error "BugInTypInference: apply non-function"
evalExp (Let bs body) = do
  let (names, exps) = unzip bs
  values <- mapM evalExp exps
  bindValues names values
  evalExp body
evalExp (Letrec bs body) = do
  let (names, exps) = unzip bs
  let lambdas = map asLambda exps
  env <- getEnv
  envRef <- newRef env
  let closures = map (`Closure` envRef) lambdas
  let newEnv = binds names closures env
  writeRef envRef newEnv
  replaceEnv newEnv
  evalExp body
evalExp (Lambda formals body) = do
  env <- getEnv
  ref <- newRef env
  return $ Closure (formals, body) ref
evalExp (Case (Literal v) ((p, e) : choices)) =
  case match p v of
    Nothing -> evalExp (Case (Literal v) choices)
    Just env' -> do
      extendEnv env'
      evalExp e
evalExp (Case (Literal v) []) = throwError $ "Runtime Error: case does not match " ++ show v
evalExp (Case scrutinee cs) = do
  v <- evalExp scrutinee
  evalExp (Case (Literal v) cs)

match :: Pattern -> Value -> Maybe (Env Value)
match (PApp c []) (ConVal c' [])
  | c == c' = Just emptyEnv
match (PApp c ps) (ConVal c' vs)
  | c == c' && length ps == length vs = do
      envs <- zipWithM match ps vs
      return $ concat envs
match (PVar x) v = Just (bind x v emptyEnv)
match Underscore _ = Just emptyEnv
match _ _ = Nothing

runEval :: EvalMonad a -> EvalState -> (Either String a, EvalState)
runEval e = runState (runExceptT e)
