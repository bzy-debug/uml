{-# LANGUAGE RankNTypes #-}

module Eval where

import Basic
import Control.Monad.Except
import Control.Monad.State
import Core
import Lens.Micro
import Lens.Micro.Extras
import Primitives
import Type

type ValueEnv = Env Value

data EnvRef = EnvRef
  { _mem :: [(Ref, ValueEnv)],
    _ref :: Ref
  }

initEnvRef :: EnvRef
initEnvRef = EnvRef [] 0

data EvalState = EvalState
  { _valueEnv :: ValueEnv,
    _envRef :: EnvRef
  }

initValueEnv :: ValueEnv
initValueEnv = map (\(name, prim, _) -> (name, Primitive prim)) primitives

initEvalState :: EvalState
initEvalState = EvalState initValueEnv initEnvRef

valueEnv :: Lens' EvalState ValueEnv
valueEnv f (EvalState e r) = (`EvalState` r) <$> f e

envRef :: Lens' EvalState EnvRef
envRef f (EvalState e r) = EvalState e <$> f r

mem :: Lens' EnvRef [(Ref, ValueEnv)]
mem f (EnvRef m r) = (`EnvRef` r) <$> f m

ref :: Lens' EnvRef Ref
ref f (EnvRef m r) = EnvRef m <$> f r

type EvalMonad = ExceptT String (State EvalState)

newRef :: ValueEnv -> EvalMonad Ref
newRef env = do
  evalState <- get
  let ref' = evalState ^. (envRef . ref)
  put $ evalState & (envRef . mem) %~ (\m -> (ref', env) : m) & (envRef . ref) %~ (+ 1)
  return ref'

writeRef :: Ref -> ValueEnv -> EvalMonad ()
writeRef r e = modify $ (envRef . mem) %~ (\m -> (r, e) : m)

readRef :: Ref -> EvalMonad ValueEnv
readRef r = do
  mem' <- gets $ view (envRef . mem)
  case lookup r mem' of
    Nothing -> throwError $ "NotFound: " ++ show r
    Just env -> return env

getEnv :: EvalMonad ValueEnv
getEnv = gets $ view valueEnv

lookupEnv :: Name -> EvalMonad Value
lookupEnv x = do
  env <- getEnv
  case lookup x env of
    Nothing -> throwError $ "NotFound: " ++ x
    Just v -> return v

bindValue :: Name -> Value -> EvalMonad ()
bindValue x v = modify $ valueEnv %~ bind x v

replaceEnv :: ValueEnv -> EvalMonad ()
replaceEnv env = modify $ valueEnv .~ env

localState :: Lens' EvalState a -> (a -> a) -> EvalMonad b -> EvalMonad b
localState len f mv = do
  initState <- gets $ view len
  modify $ len %~ f
  v <- mv
  modify $ len .~ initState
  return v

localValueEnv :: (ValueEnv -> ValueEnv) -> EvalMonad b -> EvalMonad b
localValueEnv = localState valueEnv

asLambda :: Exp -> ([Name], Exp)
asLambda (Lambda formals body) = (formals, body)
asLambda _ = error "BugInParser: rhs of letrec should be lambda"

evalDef :: Def -> EvalMonad String
evalDef (Val x e) = do
  v <- evalExp e
  bindValue x v
  return $ show v
evalDef (Valrec x e) = do
  env <- getEnv
  envRef <- newRef env
  let closure = Closure (asLambda e) envRef
  let newEnv = bind x closure env
  writeRef envRef newEnv
  replaceEnv newEnv
  return "<closure>"
evalDef (Data _ _ typedVcons) = do
  let vconValues = map valFor typedVcons
  mapM_ (uncurry bindValue) vconValues
  return ""
  where
    isFun (Forall _ tx) = isFun tx
    isFun (FunTy _ _) = True
    isFun _ = False
    valFor (k, t) =
      if isFun t
        then (k, Primitive (ConVal k))
        else (k, ConVal k [])

evalExp :: Exp -> EvalMonad Value
evalExp (Literal v) = return v
evalExp (Var x) = lookupEnv x
evalExp (VCon x) = lookupEnv x
evalExp (Apply fun args) = do
  funVal <- evalExp fun
  case funVal of
    Primitive prim -> do
      actuals <- mapM evalExp args
      return $ prim actuals
    Closure (formals, body) ref -> do
      savedEnv <- readRef ref
      actuals <- mapM evalExp args
      localValueEnv (const (binds formals actuals savedEnv)) (evalExp body)
    _ -> error "BugInTypInference: apply non-function"
evalExp (Let bs body) = do
  let (names, exps) = unzip bs
  values <- mapM evalExp exps
  localValueEnv (binds names values) (evalExp body)
evalExp (Letrec bs body) = do
  let (names, exps) = unzip bs
  let lambdas = map asLambda exps
  env <- getEnv
  envRef <- newRef env
  let closures = map (`Closure` envRef) lambdas
  let newEnv = binds names closures env
  writeRef envRef newEnv
  localValueEnv (const newEnv) (evalExp body)
evalExp (Lambda formals body) = do
  env <- getEnv
  ref <- newRef env
  return $ Closure (formals, body) ref
evalExp (Case (Literal v) ((p, e) : choices)) =
  case match p v of
    Nothing -> evalExp (Case (Literal v) choices)
    Just env' -> localValueEnv (`extend` env') (evalExp e)
evalExp (Case (Literal v) []) = throwError $ "Runtime Error: case does not match " ++ show v
evalExp (Case scrutinee cs) = do
  v <- evalExp scrutinee
  evalExp (Case (Literal v) cs)

match :: Pattern -> Value -> Maybe ValueEnv
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
