{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Convert where

import Ast
import Control.Monad.Except
import Type

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

unaryOp :: (Value -> Value) -> [Value] -> EvalMonad Value
unaryOp f [a] = return $ f a
unaryOp f args = throwError "BugInTypeInference: unary arity error"

binaryOp :: (Value -> Value -> Value) -> [Value] -> EvalMonad Value
binaryOp f [a, b] = return $ f a b
binaryOp f args = throwError "BugInTypeInference: binary arity error"

arithOp :: (Int -> Int -> Int) -> ([Value] -> EvalMonad Value)
arithOp f = binaryOp f'
  where
    f' (Num n1) (Num n2) = Num (f n1 n2)
    f' _ _ = error "BugInTypeInference: arithmetic operation on non-number value"

arithType :: Type
arithType = funType [intType, intType] intType

comparison :: (Value -> Value -> Bool) -> [Value] -> EvalMonad Value
comparison f = binaryOp (\v1 v2 -> embedBool (f v1 v2))

intCompare :: (Int -> Int -> Bool) -> [Value] -> EvalMonad Value
intCompare f = comparison f'
  where
    f' (Num n1) (Num n2) = f n1 n2
    f' _ _ = error "BugInTypeInference: arithmetic comparision on non-number value"

compType :: Type -> Type
compType x = funType [x, x] boolType

primitiveEqual :: Value -> Value -> Bool
primitiveEqual v v' =
  let noFun = error "compare function for equality"
   in case (v, v') of
        (Nil, Nil) -> True
        (Num n1, Num n2) -> n1 == n2
        (Sym v1, Sym v2) -> v1 == v2
        (Bool b1, Bool b2) -> b1 == b2
        (Pair v vs, Pair v' vs') ->
          primitiveEqual v v' && primitiveEqual vs vs'
        (Pair _ _, Nil) -> False
        (Nil, Pair _ _) -> False
        (Closure {}, _) -> noFun
        (Primitive {}, _) -> noFun
        (_, Closure {}) -> noFun
        (_, Primitive {}) -> noFun
        _ -> error "BugInTypeInference"

primitives :: [(String, Primitive, Type)]
primitives =
  [ ("+", arithOp (+), arithType),
    ("-", arithOp (-), arithType),
    ("*", arithOp (*), arithType),
    ("/", arithOp div, arithType),
    ("<", intCompare (<), compType intType),
    (">", intCompare (>), compType intType),
    ("=", comparison primitiveEqual, compType alpha),
    ("cons", binaryOp Pair, funType [alpha, listType alpha] (listType alpha)),
    ("car", unaryOp carf, funType [listType alpha] alpha),
    ("cdr", unaryOp cdrf, funType [listType alpha] (listType alpha))
  ]
  where
    carf (Pair car _) = car
    carf _ = error "RuntimeError: car"

    cdrf (Pair _ cdr) = cdr
    cdrf _ = error "RuntimeError: cdr"
