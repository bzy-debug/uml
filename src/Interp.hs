{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interp where

import Ast
import Control.Exception
import Control.Monad.State
import Convert
import qualified Data.Map as Map

data InterpException
  = NotFound Name
  | RuntimeError String
  deriving Show

instance Exception InterpException

data InterpState = InterpState
  { env :: Env,
    store :: Store,
    loc :: Int
  }

type InterpMonad = StateT InterpState (Either InterpException)

newRef :: Name -> Value -> InterpMonad ()
newRef name value = do
  InterpState {env = env, store = store, loc = loc} <- get
  put $
    InterpState
      { env = Map.insert name loc env,
        store = Map.insert loc value store,
        loc = loc + 1
      }

readRef :: Name -> InterpMonad Value
readRef name = do
  InterpState {env = env, store = store} <- get
  case Map.lookup name env of
    Nothing -> throw (NotFound name)
    Just loc ->
      case Map.lookup loc store of
        Nothing -> throw (NotFound name)
        Just val -> return val

writeRef :: Name -> Value -> InterpMonad ()
writeRef name val = do
  s@InterpState {env = env, store = store} <- get
  case Map.lookup name env of
    Nothing -> throw (NotFound name)
    Just loc -> put $ s {store = Map.insert loc val store}

modifyRef :: Name -> (Value -> Value) -> InterpMonad ()
modifyRef name f = do
  val <- readRef name
  writeRef name (f val)

eval :: Expr -> InterpMonad Value
eval (ELiteral val) = return val
eval (EVar name) = readRef name
eval (ESet name expr) = do
  val <- eval expr
  writeRef name val
  return val
eval (EIfx cond ifso ifelse) = do
  condVal <- eval cond
  eval $ if projectBool condVal then ifso else ifelse
eval (EWhilex guard body) = do
  guardVal <- eval guard
  if projectBool guardVal
    then eval body >> eval (EWhilex guard body)
    else return $ VBool False
eval (EBegin exprs) = iter exprs (VBool False)
  where
    iter [] lastVal = return lastVal
    iter (e:es) _ = eval e >>= iter es
eval (ELambda lambda) = do
  InterpState {env = env} <- get
  return $ VClosure lambda env
