module Infer where

import Assumption
import Ast
import Constraint
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Scheme
import Subst
import Type

type InferMonad = ExceptT String (ReaderT Assumption (StateT Int Identity))

freshType :: InferMonad Type
freshType = do
  counter <- get
  let freshName = "'t" ++ show counter
  put $ counter + 1
  return $ TVar freshName

solve :: Constraint -> InferMonad Subst
solve = undefined

infer :: Exp -> InferMonad (Type, Constraint)
infer = undefined
