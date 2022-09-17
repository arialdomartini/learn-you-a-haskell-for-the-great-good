module PatternMatchingSpec where

import Test.Hspec

len :: [a] -> Int
len [] = 0
len (_ : t) = 1 + len t

firstLetter :: String -> String
firstLetter [] = error "Empty string!"
firstLetter s@(f:_) = "The first letter of " ++ s ++ " is " ++ [f]

spec :: Spec
spec = do
  it "implements length with recursion and pattern matching" $ do
    let xs = [1,2,3,4] :: [Int]
    len xs `shouldBe` length xs

  it "provides as-pattern" $ do
    (firstLetter "Hello") `shouldBe` "The first letter of Hello is H"
