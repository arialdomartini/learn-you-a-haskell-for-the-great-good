module PalindromeSpec(spec) where

import Test.Hspec
import GHC.Float (int2Float)

spec :: Spec
spec = do

  it "select digits of a number" $ do
    digit 0 423 `shouldBe` 3
    digit 1 423 `shouldBe` 2
    digit 2 423 `shouldBe` 4

    digit 2 4564 `shouldBe` 5
    digit 3 12982 `shouldBe` 2


  it "number of digits" $ do
    numberOfDigits 1 `shouldBe` 1
    numberOfDigits 9 `shouldBe` 1
    numberOfDigits 10 `shouldBe` 2
    numberOfDigits 12345678 `shouldBe` 8
    numberOfDigits 101 `shouldBe` 3

  it "determines if a number is a palindrome" $ do
    isPalindrome 1        `shouldBe` True
    isPalindrome 101      `shouldBe` True
    isPalindrome 2112     `shouldBe` True
    isPalindrome 16244261 `shouldBe` True

    isPalindrome 21       `shouldBe` False
    isPalindrome 100012   `shouldBe` False
    isPalindrome 2113     `shouldBe` False

  it "bigger palindrome product of 2 3-digit numbers" $ do
    calculateLargest `shouldBe` 906609

calculateLargest :: Int
calculateLargest = largest (100,100) 0

next :: (Int, Int) -> (Int, Int)
next (x, 999) = (x+1, 100)
next (x, y) =   (x, y+1)

largest :: (Int, Int) -> Int -> Int
largest (1000, _) champion = champion
largest counter@(x,y) champion =
  let candidate = x * y
      nextChampion = if isPalindrome candidate && candidate > champion then candidate else champion in
    largest (next counter) nextChampion

isPalindrome :: Int -> Bool
isPalindrome n =
  all (\i -> digit i n == digit (specular i) n) [0..half]
  where
    tot = numberOfDigits n
    half = round (int2Float tot /  int2Float 2)
    specular i = tot - i - 1

numberOfDigits :: Int -> Int
numberOfDigits n = floor $ logBase 10 (int2Float n) + 1

digit :: Int -> Int -> Int
digit i n = floor (int2Float  n / (10^i)) `mod` 10
