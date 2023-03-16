module Main (main) where

import Test.Hspec
import Ast

main :: IO ()
main = hspec $ do
  describe "Show" $ do
    it "show symbol" $ do
      show (VSym "this") `shouldBe` "this"
