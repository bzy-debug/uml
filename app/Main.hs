module Main where

import Ast
import Control.Monad
import Interp
import Parser
import System.IO

interp :: String -> Either String String
interp s = do
  let exp = replParse s
--  scheme <- infer exp
  value <- eval' exp
--  return $ show value ++ " :: " ++ show scheme
  return $ show value

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  line <- getLine
  case interp line of
    Left s -> putStrLn s
    Right s -> putStrLn s

file :: String -> IO ()
file f = do
  src <- readFile f
  case interp src of
    Left s -> putStrLn s
    Right s -> putStrLn s
