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

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (h:t) = h : take' (n-1) t

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = (reverse' t) ++ [h]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = (a,b): zip' as bs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (h : t) = if e == h then True else elem' e t

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

  it "implements replicate as a recursive function" $ do
    replicate' 3 (5::Int) `shouldBe` replicate 3 5

  it "implements take as a recursive function" $ do
    (take' 5 [1,2,3,4,5,6,7,8] :: [Int]) `shouldBe` (take 5 [1,2,3,4,5,6,7,8])

  it "implements reverse as a recursive function" $ do
    (reverse' [1,2,3,4] :: [Int]) `shouldBe` reverse [1,2,3,4]

  it "implements repeat as a recursive function" $ do
    (take 5 (repeat' 'a')) `shouldBe` (take 5 (repeat 'a'))

  it "implements zip as a recursive function" $ do
    (zip' ['a', 'b', 'c'] ([1,2,3]::[Int])) `shouldBe` (zip ['a', 'b', 'c'] [1,2,3])

  it "implements elem as a recursive function" $ do
    let xs = ["bar", "foo", "baz"]
    (elem' "foo" xs) `shouldBe` (elem "foo" xs)
    (elem' "zop" xs) `shouldBe` (elem "zop" xs)
