module Main where

import qualified MyLib (someFunc)
import Parser

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  otherFunc
