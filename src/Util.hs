module Util where

notFoundError :: String -> a
notFoundError name = error $ "Variable Not Found: " ++ name

arityError :: Int -> Int -> a
arityError a b =
  error
    ( "expected "
        ++ show a
        ++ " but got "
        ++ show b
        ++ " arguments"
    )

typError :: String -> String -> a
typError typ v =
  error $
    "expected " ++ typ ++ " but got " ++ v
