module Kind where

data Kind
  = Star
  | Arrow Kind Kind
