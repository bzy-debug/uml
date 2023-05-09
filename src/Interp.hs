module Interp where

import Ast
import Control.Monad.Reader
import Control.Monad.State
import Desugar
import Eval
import Infer
import Lens.Micro
import Lens.Micro.Extras
import Sexp
import System.IO
import Text.Printf

data InputInteractivity = Prompting | NoPrompting

data OutputInteractivity = Echoing | NoEchoing

type InterpMode = (InputInteractivity, OutputInteractivity)

defaultMode :: InterpMode
defaultMode = (Prompting, Echoing)

data InterpState = InterpState
  { _evalState :: EvalState,
    _typeofState :: TypeofState
  }

initInterpState :: InterpState
initInterpState = InterpState initEvalState initTypeofState

evalState :: Lens' InterpState EvalState
evalState f (InterpState e t) = (`InterpState` t) <$> f e

typeofState :: Lens' InterpState TypeofState
typeofState f (InterpState e t) = InterpState e <$> f t

type InterpMonad = ReaderT InterpMode (StateT InterpState IO)

prompts :: InterpMode -> Bool
prompts (Prompting, _) = True
prompts (NoPrompting, _) = False

echos :: InterpMode -> Bool
echos (_, Echoing) = True
echos (_, NoEchoing) = False

runInterpMonad :: InterpMonad a -> InterpMode -> InterpState -> IO (a, InterpState)
runInterpMonad ia mode = runStateT (runReaderT ia mode)

getEvalState :: InterpMonad EvalState
getEvalState = gets $ view Interp.evalState

putEvalState :: EvalState -> InterpMonad ()
putEvalState es = modify $ Interp.evalState .~ es

getTypeofState :: InterpMonad TypeofState
getTypeofState = gets $ view Interp.typeofState

putTypeofState :: TypeofState -> InterpMonad ()
putTypeofState ts = modify $ Interp.typeofState .~ ts

interpCode :: Code -> InterpMonad ()
interpCode (Command c) = interpCommand c
interpCode (Definition d) = interpDef d

interpCommand :: Command -> InterpMonad ()
interpCommand (Use file) = do
  content <- lift . lift $ readFile file
  case stringToCode content of
    Left err -> lift . lift $ hPutStrLn stderr err
    Right codes -> local (const (NoPrompting, NoEchoing)) (mapM_ interpCode codes)

interpDef :: Ast.Def -> InterpMonad ()
interpDef def = do
  mode <- ask
  let coreDef = desugarDef def
  typeS <- getTypeofState
  let (result, typeS') = runTypeof (typeofDef coreDef) typeS
  case result of
    Left err -> lift . lift $ hPutStrLn stderr err
    Right typeResponse -> do
      evalS <- getEvalState
      let (result, evalS') = runEval (evalDef coreDef) evalS
      case result of
        Left err -> lift . lift $ hPutStrLn stderr err
        Right valueResponse -> do
          when (echos mode) $
            lift . lift $
              if valueResponse == ""
                then mapM_ putStrLn (lines typeResponse)
                else putStrLn (printf "%s : %s" valueResponse typeResponse)
          putTypeofState typeS'
          putEvalState evalS'

repl :: InterpMonad ()
repl = forever $ do
  mode <- ask
  when (prompts mode)
    (lift . lift $ putStr "> " >> hFlush stdout)
  string <- lift . lift $ getLine
  case stringToCode string of
    Left err -> lift . lift $ hPutStrLn stderr err
    Right code -> local (const (NoPrompting, Echoing)) (mapM_ interpCode code)
