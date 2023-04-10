module Infer where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

type InferMonad a = ExceptT String (StateT Int Identity) a
