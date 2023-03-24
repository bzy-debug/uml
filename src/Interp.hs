{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interp where

import Ast
import Control.Monad.Except
import Control.Monad.State
import Convert
import qualified Data.Map as Map
import Parser
import Text.Megaparsec

initState :: InterpState
initState = InterpState {env = Map.empty, store = Map.empty, loc = 0}

newBind :: Name -> Value -> InterpMonad ()
newBind name value = do
  InterpState {env = env, store = store, loc = loc} <- get
  put $
    InterpState
      { env = Map.insert name loc env,
        store = Map.insert loc value store,
        loc = loc + 1
      }

readBind :: Name -> InterpMonad Value
readBind name = do
  InterpState {env = env, store = store} <- get
  case Map.lookup name env of
    Nothing -> throwError NotFound
    Just loc ->
      case Map.lookup loc store of
        Nothing -> throwError NotFound
        Just val -> return val

writeBind :: Name -> Value -> InterpMonad ()
writeBind name val = do
  s@InterpState {env = env, store = store} <- get
  case Map.lookup name env of
    Nothing -> throwError NotFound
    Just loc -> put $ s {store = Map.insert loc val store}

modifyBind :: Name -> (Value -> Value) -> InterpMonad ()
modifyBind name f = do
  val <- readBind name
  writeBind name (f val)

primitives :: [Primitive]
primitives = []

eval :: Expr -> InterpMonad Value
eval expr = do
  s@InterpState {env = curEnv} <- get
  res <- ev expr
  put s {env = curEnv}
  return res
  where
    ev (ELiteral val) = return val
    ev (EVar name) = readBind name
    ev (ESet name expr) = do
      val <- ev expr
      writeBind name val
      return val
    ev (EIfx cond ifso ifelse) = do
      condVal <- ev cond
      ev $ if projectBool condVal then ifso else ifelse
    ev (EWhilex guard body) = do
      guardVal <- ev guard
      if projectBool guardVal
        then ev body >> ev (EWhilex guard body)
        else return $ VBool False
    ev (EBegin exprs) = iter exprs (VBool False)
      where
        iter [] lastVal = return lastVal
        iter (e : es) _ = ev e >>= iter es
    ev (ELambda names body) = do
      InterpState {env = env} <- get
      return $ VClosure names body env
    ev e@(EApply fun args) = do
      funVal <- ev fun
      case funVal of
        VPrimitve prim -> do
          vals <- mapM ev args
          prim e vals
        VClosure formals body savedEnv -> do
          actuals <- mapM ev args
          if length formals == length actuals
            then do
              modify (\s -> s {env = savedEnv})
              zipWithM_ newBind formals actuals
              ev body
            else throwError ArityError
        _ -> throwError TypeError
    ev (ELetx Let binds body) = do
      let (names, rhs) = unzip binds
      vals <- mapM ev rhs
      zipWithM_ newBind names vals
      ev body
    ev (ELetx LetStar binds body) = do
      forM_
        binds
        ( \(name, rhs) -> do
            val <- ev rhs
            newBind name val
        )
      ev body
    ev (ELetx LetRec binds body) = do
      let (names, rhs) = unzip binds
      mapM_ (`newBind` VNil) names
      vals <- mapM ev rhs
      zipWithM_ writeBind names vals
      ev body

testEval :: String -> Except InterpException Value
testEval s =
  case parseMaybe expression s of
    Nothing -> error "Parse error"
    Just expr -> evalStateT (eval expr) initState
