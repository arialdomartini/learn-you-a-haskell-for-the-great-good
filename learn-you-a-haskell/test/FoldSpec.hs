module FoldSpec where

import Test.Hspec

sum' :: Num a => [a] -> a
sum' = foldl1 (+)

maximum' :: Ord a => [a] -> a
maximum' = foldl1 (max) -- does not work with empty lists!

spec :: Spec
spec = do
  it "sums elements in a list" $ do
    (sum' [1,2,3]) `shouldBe` (6 :: Int)

  it "maximum with foldl" $ do
    (maximum' [1,5,6,2,44,2]) `shouldBe` (44 :: Int)
    (maximum' [1]) `shouldBe` (1 :: Int)
