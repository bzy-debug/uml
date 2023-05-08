module Primitives where

import Core
import Scheme
import Type

intType :: Type
intType = TCon $ TypeCons "int" 0

symType :: Type
symType = TCon $ TypeCons "sym" 1

arrowType :: Type
arrowType = TCon $ TypeCons "->" 2

argType :: Type
argType = TCon $ TypeCons "arg" 3

funType :: [Type] -> Type -> Type
funType args ret = TApp arrowType [TApp argType args, ret]

alpha :: Type
alpha = TVar "'a"

boolType :: Type
boolType = TCon $ TypeCons "bool" 4

true :: Value
true = ConVal "#t" []

false :: Value
false = ConVal "#f" []

embedInt :: Int -> Value
embedInt = Num

projectInt :: Value -> Maybe Int
projectInt (Num n) = Just n
projectInt _ = Nothing

embedBool :: Bool -> Value
embedBool True = true
embedBool False = false

projectBool :: Value -> Bool
projectBool (ConVal "#t" []) = True
projectBool (ConVal "#f" []) = False
projectBool _ = error "BugInTypeInference: project bool"

unaryOp :: (Value -> Value) -> Primitive
unaryOp f [a] = f a
unaryOp _ _ = error "BugInTypeInference: unary arity error"

binaryOp :: (Value -> Value -> Value) -> Primitive
binaryOp f [a, b] = f a b
binaryOp _ _ = error "BugInTypeInference: binary arity error"

arithOp :: (Int -> Int -> Int) -> Primitive
arithOp f = binaryOp f'
  where
    f' :: Value -> Value -> Value
    f' (Num n1) (Num n2) = Num (f n1 n2)
    f' _ _ = error "BugInTypeInference: arithmetic operation on non-number value"

comparison :: (Value -> Value -> Bool) -> Primitive
comparison f = binaryOp f'
  where
    f' :: Value -> Value -> Value
    f' v1 v2 = embedBool $ f v1 v2

intCompare :: (Int -> Int -> Bool) -> Primitive
intCompare f = binaryOp f'
  where
    f' :: Value -> Value -> Value
    f' (Num n1) (Num n2) = embedBool $ f n1 n2
    f' _ _ = error "BugInTypeInference: arithmetic comparision on non-number value"

primitiveEqual :: Value -> Value -> Bool
primitiveEqual v v' =
  let noFun = error "compare function for equality"
   in case (v, v') of
        (Num n1, Num n2) -> n1 == n2
        (Sym v1, Sym v2) -> v1 == v2
        (ConVal c1 vs1, ConVal c2 vs2) ->
          c1 == c2 && and (zipWith primitiveEqual vs1 vs2)
        (Closure {}, _) -> noFun
        (Primitive {}, _) -> noFun
        (_, Closure {}) -> noFun
        (_, Primitive {}) -> noFun
        _ -> error "BugInTypeInference: compare"

monoType :: Type -> Scheme
monoType = Scheme []

polyAlpha :: Type -> Scheme
polyAlpha = Scheme ["'a"]

primitives :: [(String, Primitive, Scheme)]
primitives =
  [ ("+", arithOp (+), monoType $ funType [intType, intType] intType),
    ("-", arithOp (-), monoType $ funType [intType, intType] intType),
    ("*", arithOp (*), monoType $ funType [intType, intType] intType),
    ("/", arithOp div, monoType $ funType [intType, intType] intType),
    ("<", intCompare (<), monoType $ funType [intType, intType] boolType),
    (">", intCompare (>), monoType $ funType [intType, intType] boolType),
    ("=", comparison primitiveEqual, polyAlpha $ funType [alpha, alpha] boolType)
  ]
