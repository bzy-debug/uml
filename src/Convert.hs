{-# OPTIONS_GHC -Wno-unused-matches #-}

module Convert where

import Ast
import Control.Monad (liftM2)

embedInt :: Int -> Value
embedInt = Num

projectInt :: Value -> Maybe Int
projectInt (Num n) = Just n
projectInt _ = Nothing

embedBool :: Bool -> Value
embedBool = Bool

projectBool :: Value -> Bool
projectBool (Bool False) = False
projectBool _ = True

embedList :: [Value] -> Value
embedList = foldr Pair Nil

projectList :: Value -> Maybe [Value]
projectList (Pair car cdr) = liftM2 (:) (Just car) (projectList cdr)
projectList Nil = Just []
projectList _ = Nothing

-- embedArith :: (Int -> Int -> Int) -> Value -> Value -> Value
-- embedArith f (VNum n1) (VNum n2) = VNum $ f n1 n2
-- embedArith f (VNum _) v = typError "integer: " (show v)
-- embedArith f v _ = typError "integer: " (show v)

-- unaryOp :: (Value -> Value) -> [Value] -> Value
-- unaryOp f [a] = f a
-- unaryOp f args = arityError 1 (length args)

-- binaryOp :: (Value -> Value -> Value) -> [Value] -> Value
-- binaryOp f [a, b] = f a b
-- binaryOp f args = arityError 2 (length args)
