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

unaryOp :: (Value -> EvalMonad Value) -> Primitive
unaryOp f [a] = f a
unaryOp f args = throwError "BugInTypeInference: unary arity error"

binaryOp :: (Value -> Value -> EvalMonad Value) -> Primitive
binaryOp f [a, b] = f a b
binaryOp f args = throwError "BugInTypeInference: binary arity error"

arithOp :: (Int -> Int -> Int) -> Primitive
arithOp f = binaryOp f'
  where
    f' :: Value -> Value -> EvalMonad Value
    f' (Num n1) (Num n2) = return $ Num (f n1 n2)
    f' _ _ = throwError "BugInTypeInference: arithmetic operation on non-number value"

boolUnaryOp :: (Bool -> Bool) -> Primitive
boolUnaryOp f = unaryOp f'
  where
    f' :: Value -> EvalMonad Value
    f' (Bool b) = return $ Bool (f b)
    f' _ = throwError "BugInTypeInference: bool operation on non-bool value"

boolBinOp :: (Bool -> Bool -> Bool) -> Primitive
boolBinOp f = binaryOp f'
  where
    f' :: Value -> Value -> EvalMonad Value
    f' (Bool b1) (Bool b2) = return $ Bool (f b1 b2)
    f' _ _ = throwError "BugInTypeInference: bool operation on non-bool value"

boolUnaryTy :: Type
boolUnaryTy = funType [boolType] boolType

boolBinTy :: Type
boolBinTy = funType [boolType, boolType] boolType

arithType :: Type
arithType = funType [intType, intType] intType

comparison :: (Value -> Value -> EvalMonad Bool) -> Primitive
comparison f = binaryOp f'
  where
    f' :: Value -> Value -> EvalMonad Value
    f' v1 v2 = embedBool <$> f v1 v2

intCompare :: (Int -> Int -> Bool) -> Primitive
intCompare f = binaryOp f'
  where
    f' :: Value -> Value -> EvalMonad Value
    f' (Num n1) (Num n2) = return . embedBool $ f n1 n2
    f' _ _ = throwError "BugInTypeInference: arithmetic comparision on non-number value"

compType :: Type -> Type
compType x = funType [x, x] boolType

primitiveEqual :: Value -> Value -> EvalMonad Bool
primitiveEqual v v' =
  let noFun = throwError "compare function for equality"
   in case (v, v') of
        (Nil, Nil) -> return True
        (Num n1, Num n2) -> return $ n1 == n2
        (Sym v1, Sym v2) -> return $ v1 == v2
        (Bool b1, Bool b2) -> return $ b1 == b2
        (Pair v vs, Pair v' vs') -> liftM2 (&&) (primitiveEqual v v') (primitiveEqual vs vs')
        (Pair _ _, Nil) -> return False
        (Nil, Pair _ _) -> return False
        (Closure {}, _) -> noFun
        (Primitive {}, _) -> noFun
        (_, Closure {}) -> noFun
        (_, Primitive {}) -> noFun
        _ -> throwError "BugInTypeInference: compare"

primitives :: [(String, Primitive, Type)]
primitives =
  [ ("+", arithOp (+), arithType),
    ("-", arithOp (-), arithType),
    ("*", arithOp (*), arithType),
    ("/", arithOp div, arithType),
    ("<", intCompare (<), compType intType),
    (">", intCompare (>), compType intType),
    ("=", comparison primitiveEqual, compType alpha),
    ("and", boolBinOp (&&), boolBinTy),
    ("or", boolBinOp (||), boolBinTy),
    ("not", boolUnaryOp not, boolUnaryTy),
    ("cons", binaryOp pair, funType [alpha, listType alpha] (listType alpha)),
    ("car", unaryOp carf, funType [listType alpha] alpha),
    ("cdr", unaryOp cdrf, funType [listType alpha] (listType alpha))
  ]
  where
    pair v v' = return $ Pair v v'
    carf :: Value -> EvalMonad Value
    carf (Pair car _) = return car
    carf _ = throwError "RuntimeError: car"
    cdrf :: Value -> EvalMonad Value
    cdrf (Pair _ cdr) = return cdr
    cdrf _ = throwError "RuntimeError: cdr"
