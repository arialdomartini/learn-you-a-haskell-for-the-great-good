module PatternMatchingSpec where

import Test.Hspec

len :: [a] -> Int
len [] = 0
len (_ : t) = 1 + len t

spec :: Spec
spec = do
  it "implements length with recursion and pattern matching" $ do
    let xs = [1,2,3,4] :: [Int]
    len xs `shouldBe` length xs
