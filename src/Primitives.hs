module Primitives where

import Core
import Scheme
import Type

embedInt :: Int -> Value
embedInt = Num

projectInt :: Value -> Maybe Int
projectInt (Num n) = Just n
projectInt _ = Nothing

embedBool :: Bool -> Value
embedBool True = ConVal "#t" []
embedBool False = ConVal "#f" []

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

-- primitiveEqual :: Value -> Value -> EvalMonad Bool
-- primitiveEqual v v' =
--   let noFun = throwError "compare function for equality"
--    in case (v, v') of
-- --        (Nil, Nil) -> return True
--         (Num n1, Num n2) -> return $ n1 == n2
--         (Sym v1, Sym v2) -> return $ v1 == v2
-- --        (Bool b1, Bool b2) -> return $ b1 == b2
-- --        (Pair v vs, Pair v' vs') -> liftM2 (&&) (primitiveEqual v v') (primitiveEqual vs vs')
-- --        (Pair _ _, Nil) -> return False
-- --        (Nil, Pair _ _) -> return False
--         (Closure {}, _) -> noFun
--         (Primitive {}, _) -> noFun
--         (_, Closure {}) -> noFun
--         (_, Primitive {}) -> noFun
--         _ -> error "BugInTypeInference: compare"

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
    (">", intCompare (>), monoType $ funType [intType, intType] boolType)
--    ("=", comparison primitiveEqual, polyAlpha $ funType [alpha, alpha] boolType)
  ]
