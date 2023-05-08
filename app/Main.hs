module Main where

import Sexp
import Interp

main :: IO ()
main = do
  putStrLn "Hello World"
  string <- readFile "./test"
  case stringToCode string of
    Left err -> putStrLn err
    Right codes ->
      fst <$> runInterpMonad (mapM_ interpCode codes) defaultMode initInterpState
