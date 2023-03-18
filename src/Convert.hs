module Convert where

import Ast
import Control.Monad (liftM2)

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
