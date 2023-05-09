module Main where

import Interp
import Sexp
import System.Environment

interp a = runInterpMonad a defaultMode initInterpState

main :: IO ()
main = do
  putStrLn "Welcome to this language"
  args <- getArgs
  if not (null args)
    then do
      string <- readFile $ head args
      case stringToCode string of
        Left err -> putStrLn err
        Right codes -> do
          fst <$> interp (mapM_ interpCode codes >> repl)
    else fst <$> interp repl
