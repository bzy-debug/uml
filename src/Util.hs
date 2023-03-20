module Util where
import Control.Exception (Exception)

data InterpException
  = NotFound String
  | RuntimeError String
  deriving (Show)

instance Exception InterpException
