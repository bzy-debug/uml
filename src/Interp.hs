{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interp where

import Ast
import Control.Exception
import Control.Monad.State
import Convert
import qualified Data.Map as Map
import qualified Data.Text as T
import Parser (Parser, expression)
import Text.Megaparsec (parseMaybe)
import Util

data InterpState = InterpState
  { env :: Env,
    store :: Store,
    loc :: Int
  }

initState :: InterpState
initState = InterpState {env = Map.empty, store = Map.empty, loc = 0}

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
eval expr = do
  s@InterpState {env = curEnv} <- get
  res <- ev expr
  put s {env = curEnv}
  return res
  where
    ev (ELiteral val) = return val
    ev (EVar name) = readRef name
    ev (ESet name expr) = do
      val <- ev expr
      writeRef name val
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
    ev (ELambda lambda) = do
      InterpState {env = env} <- get
      return $ VClosure lambda env
    ev e@(EApply fun args) = do
      funVal <- ev fun
      case funVal of
        VPrimitve prim -> do
          vals <- mapM ev args
          either throw return (prim e vals)
        VClosure (formals, body) savedEnv -> do
          actuals <- mapM ev args
          if length actuals == length formals
            then do
              modify (\s -> s {env = savedEnv})
              zipWithM_ newRef formals actuals
              ev body
            else throw (RuntimeError "arity number error")
        v -> throw (RuntimeError $ "Not a function" ++ show v)
    ev (ELetx Let binds body) = do
      let (names, rhs) = unzip binds
      vals <- mapM ev rhs
      zipWithM_ newRef names vals
      ev body
    ev (ELetx LetStar binds body) = do
      forM_
        binds
        ( \(name, rhs) -> do
            val <- ev rhs
            newRef name val
        )
      ev body
    ev (ELetx LetRec binds body) = do
      let (names, rhs) = unzip binds
      mapM_ (`newRef` VNil) names
      vals <- mapM ev rhs
      zipWithM_ writeRef names vals
      ev body

parseEither :: Parser Expr -> T.Text -> Either InterpException Expr
parseEither p s = maybe (Left $ RuntimeError "ParseError") Right (parseMaybe p s)

testEval :: T.Text -> Either InterpException Value
testEval s = do
  expr <- parseEither expression s
  evalStateT (eval expr) initState
