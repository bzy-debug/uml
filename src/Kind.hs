module Kind where

data Kind
  = Star
  | Arrow [Kind] Kind

instance Eq Kind where
  (==) Star Star = True
  (==) (Arrow args result) (Arrow args' result') =
    and (zipWith (==) args args') && result == result'
  (==) _ _ = False

instance Show Kind where
  show Star = "*"
  show (Arrow args result) = "( => " ++ "[" ++ unwords (map show args) ++ "] " ++ show result ++ ")"
