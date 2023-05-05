module Interp where

import Ast
import Control.Monad.Reader
import Control.Monad.State
import Eval
import Sexp

type InterpMonad = ReaderT InterpMode (StateT EvalState IO)

data InterpMode = Interactive | NonInteractive

runInterpMonad :: InterpMonad a -> InterpMode -> EvalState -> IO (a, EvalState)
runInterpMonad ia mode = runStateT (runReaderT ia mode)

interpCode :: Code -> InterpMonad ()
interpCode (Command c) = interpCommand c
interpCode (Definition d) = interpDef d
interpCode (Expression e) = interpExp e

interpCommand :: Command -> InterpMonad ()
interpCommand (Use file) = do
  content <- lift . lift $ readFile file
  case stringToCode content of
    Left err -> lift . lift $ putStrLn err
    Right codes -> mapM_ interpCode codes
interpCommand (Check e v) = do
  s <- get
  let (res, s1) = runEval (evalExp e) s
  put s1
  case res of
    Left err -> lift . lift $ putStrLn err
    Right v' -> do
      if v == v'
        then lift . lift $ putStrLn "check pass"
        else lift . lift $ putStrLn ("Expect " ++ show v ++ " but got " ++ show v')

interpExp :: Exp -> InterpMonad ()
interpExp exp = do
  s <- get
  let (res, s1) = runEval (evalExp exp) s
  put s1
  case res of
    Left err -> lift . lift $ putStrLn err
    Right a -> do
      mode <- ask
      case mode of
        Interactive -> lift . lift $ print a
        NonInteractive -> return ()

interpDef :: Def -> InterpMonad ()
interpDef def = do
  s <- get
  let (res, s1) = runEval (evalDef def) s
  put s1
  case res of
    Left err -> lift . lift $ putStrLn err
    Right a -> lift . lift $ putStrLn a
