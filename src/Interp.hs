module Interp where

import Ast
import Control.Monad.Reader
import Control.Monad.State
import Desugar
import Eval
import Infer
import Lens.Micro
import Lens.Micro.Extras
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
interpCode (Command _) = error "TODO" -- interpCommand c
interpCode (Definition d) = interpDef d

-- interpCommand :: Command -> InterpMonad ()
-- interpCommand (Use file) = do
--   content <- lift . lift $ readFile file
--   case stringToCode content of
--     Left err -> lift . lift $ putStrLn err
--     Right codes -> mapM_ interpCode codes
-- interpCommand (Check e v) = do
--   s <- get
--   let (res, s1) = runEval (evalExp e) s
--   put s1
--   case res of
--     Left err -> lift . lift $ putStrLn err
--     Right v' -> do
--       if v == v'
--         then lift . lift $ putStrLn "check pass"
--         else lift . lift $ putStrLn ("Expect " ++ show v ++ " but got " ++ show v')

interpDef :: Ast.Def -> InterpMonad ()
interpDef def = do
  let coreDef = desugarDef def
  -- lift . lift $ print coreDef
  typeS <- getTypeofState
  let (result, typeS') = runTypeof (typeofDef coreDef) typeS
  case result of
    Left err -> lift . lift $ putStrLn err
    Right typeResponse -> do
      evalS <- getEvalState
      let (result, evalS') = runEval (evalDef coreDef) evalS
      case result of
        Left err -> lift . lift $ putStrLn err
        Right valueResponse -> do
          lift . lift $
            if valueResponse == ""
              then mapM_ putStrLn (lines typeResponse)
              else putStrLn (printf "%s : %s" valueResponse typeResponse)
          putTypeofState typeS'
          putEvalState evalS'
