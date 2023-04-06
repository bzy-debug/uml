module Interp where

import Ast
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Convert

initialState :: RefState
initialState = RefState {ref = 0, mem = []}

primitiveEnv :: Env Value
primitiveEnv = map (\(name, prim, _) -> (name, Primitive prim)) primitives

newRef :: Env Value -> EvalMonad Ref
newRef env = do
  RefState {mem = mem, ref = ref} <- get
  put $
    RefState
      { mem = (ref, env) : mem,
        ref = ref + 1
      }
  return ref

writeRef :: Ref -> Env Value -> EvalMonad ()
writeRef r e = do
  RefState {mem = mem, ref = ref} <- get
  put $ RefState {mem = (r, e) : mem, ref = ref}

readRef :: Ref -> EvalMonad (Env Value)
readRef r = do
  RefState {mem = mem} <- get
  case lookup r mem of
    Nothing -> throwError "NotFound"
    Just env -> return env

lookupEnv :: Name -> EvalMonad Value
lookupEnv x = do
  env <- ask
  case lookup x env of
    Nothing -> throwError $ "NotFound: " ++ x
    Just v -> return v

eval :: Exp -> EvalMonad Value
eval (Literal v) = return v
eval (Var x) = lookupEnv x
eval (If cond ifso ifelse) = do
  condVal <- eval cond
  eval $ if projectBool condVal then ifso else ifelse
eval (Begin exps) =
  let iter [] lastVal = lastVal
      iter (e : es) _ = iter es (eval e)
   in iter exps (return $ Bool False)
eval (Apply fun args) = do
  funVal <- eval fun
  case funVal of
    Primitive prim -> do
      actuals <- mapM eval args
      prim actuals
    Closure (formals, body) ref -> do
      env <- readRef ref
      actuals <- mapM eval args
      local (\_ -> binds formals actuals env) (eval body)
    _ -> throwError "BugInTypInference: apply non-function"
eval (Letx Let bs body) = do
  let (names, exps) = unzip bs
  values <- mapM eval exps
  local (binds names values) (eval body)
eval (Letx LetStar bs body) =
  case bs of
    [] -> eval body
    b : bs -> eval (Letx Let [b] (Letx LetStar bs body))
eval (Letx LetRec bs body) =
  let asLambda :: Exp -> EvalMonad ([Name], Exp)
      asLambda (Lambda formals body) = return (formals, body)
      asLambda _ = throwError "ParseError: rhs of letrec should be lambda"
   in do
        let (names, exps) = unzip bs
        lambdas <- mapM asLambda exps
        env <- ask
        envRef <- newRef env
        let closures = map (`Closure` envRef) lambdas
        let newEnv = binds names closures env
        writeRef envRef newEnv
        local (const newEnv) (eval body)
eval (Lambda formals body) = do
  env <- ask
  ref <- newRef env
  return $ Closure (formals, body) ref

runEval :: EvalMonad a -> (Either String a, RefState)
runEval e = runIdentity (runStateT (runReaderT (runExceptT e) primitiveEnv) initialState)

eval' :: Exp -> Either String String
eval' exp = case fst (runEval (eval exp)) of
  Left err -> Left err
  Right v -> Right $ show v
