module Chapter10.ReversePolishNotationCalculatorSpec where

import Chapter10.ReversePolishNotationCalculator
import Test.Hspec

spec = do
  -- it "just passes" $ do
  --   calculate [Numero 1] `shouldBe` 1
  -- it "calculates a sum" $ do
  --   calculate [Numero 1, Numero 2, Plus] `shouldBe` 3
  -- it "calculates a series of sums" $ do
  --   calculate [Numero 1, Numero 2, Numero 3, Plus, Plus] `shouldBe` 6


  it "breaks down an expression into a list" $ do
    tokenize "10 20 3 + *" `shouldBe` ["10", "20", "3", "+", "*"] 

  it "expression with literal value" $ do
    rpn "5" `shouldBe` 5

  it "calculates sums" $ do
    rpn "2 3 +" `shouldBe` 5

  it "calculate sums and products" $ do
    rpn "2 3 + 1 1 + *" `shouldBe` 10

