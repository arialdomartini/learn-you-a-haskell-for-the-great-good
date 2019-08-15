module Chapter10.ReversePolishNotationCalculatorSpec where

import Chapter10.ReversePolishNotationCalculator
import Test.Hspec

spec = do
  it "just passes" $ do
    calculate [Numero 1] `shouldBe` 1
  it "calculates a sum" $ do
    calculate [Numero 1, Numero 2, Plus] `shouldBe` 3
  it "calculates a series of sums" $ do
    calculate [Numero 1, Numero 2, Numero 3, Plus, Plus] `shouldBe` 6
