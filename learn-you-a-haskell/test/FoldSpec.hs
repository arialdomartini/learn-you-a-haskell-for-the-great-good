module FoldSpec where

import Test.Hspec

sum' :: Num a => [a] -> a
sum' = foldl1 (+)

spec :: Spec
spec = do
  it "sums elements in a list" $ do
    (sum' [1,2,3]) `shouldBe` (6 :: Int)
