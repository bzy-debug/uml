module Util where
import Control.Exception (Exception)
import qualified Data.Text as T

data InterpException
  = NotFound T.Text
  | RuntimeError String
  deriving (Show)

instance Exception InterpException
