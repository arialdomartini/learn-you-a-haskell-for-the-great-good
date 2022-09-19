module FoldSpec where

import Test.Hspec

sum' :: Num a => [a] -> a
sum' = foldl1 (+)

maximum' :: Ord a => [a] -> a
maximum' = foldl1 (max) -- does not work with empty lists!

reverse' :: [a] -> [a]
reverse' = foldl (\a x -> x : a) []

product' :: Num a => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x a -> if f x then x : a else a) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []


-- foldr
-- f 1 (f 2 (f 3 (f 4)))

-- foldr
-- f (f (f 1 2) 3 4)

spec :: Spec
spec = do
  it "sums elements in a list" $ do
    (sum' [1,2,3]) `shouldBe` (6 :: Int)

  it "maximum with foldl" $ do
    (maximum' [1,5,6,2,44,2]) `shouldBe` (44 :: Int)
    (maximum' [1]) `shouldBe` (1 :: Int)

  it "reverse implemented with foldl" $ do
    (reverse' ['a', 'b', 'c']) `shouldBe` ['c', 'b', 'a']

  it "product implemented with foldl" $ do
    (product' [1,2,3,4]) `shouldBe` ((1 * 2 * 3 * 4) :: Int)

  it "filter implemeted with foldr" $ do
    (filter' even [1,2,3,4]) `shouldBe` ([2,4] :: [Int])

  it "last implemented with foldl" $ do
    (last' ['a', 'b', 'c']) `shouldBe` 'c'

  it "implement reverse with flip (:)" $ do
    (reverse'' ['a', 'b', 'c']) `shouldBe` ['c', 'b', 'a']
