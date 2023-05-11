module Main where

import Interp
import Sexp
import System.Environment
import System.IO

main :: IO ()
main = do
  putStrLn "Welcome to uml"
  args <- getArgs
  if not (null args)
    then runFile (head args) >>= interp_ repl defaultMode
    else basisInterpState >>= interp_ repl defaultMode

runFile :: String -> IO InterpState
runFile filePath = do
  string <- readFile filePath
  case stringToCode string of
    Left err -> hPutStrLn stderr err >> basisInterpState
    Right codes ->
      basisInterpState >>= interp (mapM_ interpCode codes) silentMode
