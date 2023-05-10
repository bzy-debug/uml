module Main where

import Interp
import Sexp
import System.Environment
import System.IO

main :: IO ()
main = do
  case stringToCode basis of
    Left err -> hPutStrLn stderr err
    Right codes -> do
      basisInterpState <- snd <$> runInterpMonad (mapM_ interpCode codes) (NoPrompting, NoEchoing) initInterpState
      putStrLn "Welcome to uml"
      args <- getArgs
      if not (null args)
        then do
          string <- readFile $ head args
          case stringToCode string of
            Left err -> hPutStrLn stderr err
            Right codes -> do
              fst <$> runInterpMonad (mapM_ interpCode codes >> repl) defaultMode basisInterpState
        else fst <$> runInterpMonad repl defaultMode basisInterpState
