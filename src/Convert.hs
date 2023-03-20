{-# OPTIONS_GHC -Wno-unused-matches #-}

module Convert where

import Ast
import Control.Monad (liftM2)
import Util

embedInt :: Int -> Value
embedInt = VNum

projectInt :: Value -> Maybe Int
projectInt (VNum n) = Just n
projectInt _ = Nothing

embedBool :: Bool -> Value
embedBool = VBool

projectBool :: Value -> Bool
projectBool (VBool False) = False
projectBool _ = True

embedList :: [Value] -> Value
embedList = foldr VPair VNil

projectList :: Value -> Maybe [Value]
projectList (VPair car cdr) = liftM2 (:) (Just car) (projectList cdr)
projectList VNil = Just []
projectList _ = Nothing

embedArith :: (Int -> Int -> Int) -> Value -> Value -> Value
embedArith f (VNum n1) (VNum n2) = VNum $ f n1 n2
embedArith f (VNum _) v = typError "integer: " (show v)
embedArith f v _ = typError "integer: " (show v)

unaryOp :: (Value -> Value) -> [Value] -> Value
unaryOp f [a] = f a
unaryOp f args = arityError 1 (length args)

binaryOp :: (Value -> Value -> Value) -> [Value] -> Value
binaryOp f [a, b] = f a b
binaryOp f args = arityError 2 (length args)
