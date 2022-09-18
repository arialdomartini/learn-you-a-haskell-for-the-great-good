module RecursionSpec where

import Test.Hspec

fibn :: Int -> Int
fibn 0 = 1
fibn 1 = 1
fibn n = fibn (n - 1) + fibn (n - 2)

fib :: Int -> [Int]
fib n = fmap fibn [1..n]

myMax :: [Int] -> Int
myMax xs =
  max' xs (minBound :: Int)
  where
    max' [] m = m
    max' (x:t) m = if x > m then max' t x else max' t m

myMax' :: [Int] -> Int
myMax' [] = error "Cannot calculate the maximum element of an empty list"
myMax' [m] = m
myMax' (x:t) =
  greatestBetween x (myMax' t)
  where greatestBetween a b = if a > b then a else b

replicate' :: Int -> a -> [a]
replicate' n e = e : replicate (n - 1) e

spec :: Spec
spec = do
  it "calculates the fibonacci series" $ do
    (fib 10) `shouldBe` [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]

  it "calculates the maximum of a list using recursion" $ do
    let xs = [1,6,2,66,66,12,54] :: [Int]
      in (myMax xs) `shouldBe` (maximum xs)

  it "calculates the maximum of a list using a simplified recursion" $ do
    let xs = [1,6,2,66,66,12,54] :: [Int]
      in (myMax' xs) `shouldBe` (maximum xs)

  it "implement replicate as a recursive function" $ do
    replicate' 3 (5::Int) `shouldBe` replicate 3 5
