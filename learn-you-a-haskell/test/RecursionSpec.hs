module RecursionSpec where

import Test.Hspec

fibn :: Int -> Int
fibn 0 = 1
fibn 1 = 1
fibn n = fibn (n - 1) + fibn (n - 2)

fib :: Int -> [Int]
fib n = fmap fibn [1..n]

spec :: Spec
spec = do
  it "calculates the fibonacci series" $ do
    (fib 10) `shouldBe` [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
