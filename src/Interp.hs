module Interp where

import Ast
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Lens.Micro
import Lens.Micro.Extras
import Primitives

initEvalState :: EvalState
initEvalState =
  EvalState
    (map (\(name, prim, _) -> (name, Primitive prim)) primitives)
    (EnvRef [] 0)

newRef :: Env Value -> EvalMonad Ref
newRef env = do
  evalState <- get
  let ref' = view (envRef . ref) evalState
  put $ evalState & (envRef . mem) %~ (\m -> (ref', env) : m) & (envRef . ref) %~ (+1)
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

evalDef :: Def -> EvalMonad ()
evalDef def =
  case def of
    (Val x e) -> evalExp e >>= bindValue x
    (Valrec x e) -> evalExp e >>= bindValue x 
    (DExp e) -> evalExp e >>= bindValue "it"
    (Define f xs body) -> evalDef (Valrec f (Lambda xs body))

evalExp :: Exp -> EvalMonad Value
evalExp (Literal v) = return v
evalExp (Var x) = lookupEnv x
evalExp (If cond ifso ifelse) = do
  condVal <- evalExp cond
  evalExp $ if projectBool condVal then ifso else ifelse
evalExp (Begin exps) =
  let iter [] lastVal = lastVal
      iter (e : es) _ = iter es (evalExp e)
   in iter exps (return $ Bool False)
evalExp (Apply fun args) = do
  funVal <- evalExp fun
  case funVal of
    Primitive prim -> do
      actuals <- mapM evalExp args
      prim actuals
    Closure (formals, body) ref -> do
      savedEnv <- readRef ref
      actuals <- mapM evalExp args
      modify $ valueEnv .~ binds formals actuals savedEnv
      evalExp body
    _ -> throwError "BugInTypInference: apply non-function"
evalExp (Letx Let bs body) = do
  let (names, exps) = unzip bs
  values <- mapM evalExp exps
  modify $ valueEnv %~ binds names values
  evalExp body
evalExp (Letx LetStar bs body) =
  case bs of
    [] -> evalExp body
    b : bs -> evalExp (Letx Let [b] (Letx LetStar bs body))
evalExp (Letx LetRec bs body) =
  let asLambda :: Exp -> EvalMonad ([Name], Exp)
      asLambda (Lambda formals body) = return (formals, body)
      asLambda _ = throwError "SyntaxError: rhs of letrec should be lambda"
   in do
        let (names, exps) = unzip bs
        lambdas <- mapM asLambda exps
        env <- gets $ view valueEnv
        envRef <- newRef env
        let closures = map (`Closure` envRef) lambdas
        let newEnv = binds names closures env
        writeRef envRef newEnv
        modify $ valueEnv .~ newEnv
        evalExp body
evalExp (Lambda formals body) = do
  env <- gets $ view valueEnv
  ref <- newRef env
  return $ Closure (formals, body) ref

runEval :: EvalMonad a -> EvalState -> (Either String a, EvalState)
runEval e state = runIdentity (runStateT (runExceptT e) state)

evalExp' :: Exp -> Either String String
evalExp' exp = case fst (runEval (evalExp exp) initEvalState) of
  Left err -> Left err
  Right v -> Right $ show v
