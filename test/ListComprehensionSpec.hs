module ListComprehensionSpec(spec) where

import Test.Hspec

spec :: Spec
spec = do


  it "can use list comprehensions" $ do
    let lc =     [(x * 2, y) | x <- [1..10], y <- [1..10], y `mod` 2 == 0] :: [(Int, Int)]
    let manual = buildList
    lc `shouldBe` manual


  it "calculate fizz buzz" $ do
    let fb = [ fzbz x | x <- [1..10]]
    fb `shouldBe` ["1", "fizz", "buzz", "fizz", "5", "fizzbuzz", "7", "fizz", "buzz", "fizz"]


  it "calculate fizz buzz with foldl" $ do
    let fb = [ fzbz_foldl x | x <- [1..10]]
    fb `shouldBe` ["1", "fizz", "buzz", "fizz", "5", "fizzbuzz", "7", "fizz", "buzz", "fizz"]


  it "introduce people" $ do
    let people = ["Harry", "Sally", "John", "Sally"]
        ns = [0..(length people -1)]
        np = zip ns people
        greetings = [a ++ ", this is " ++ b | (n, a) <- np, (m, b) <- np, n /= m]
    greetings `shouldBe` [
      "Harry, this is Sally",
      "Harry, this is John",
      "Harry, this is Sally",

      "Sally, this is Harry",
      "Sally, this is John",
      "Sally, this is Sally",

      "John, this is Harry",
      "John, this is Sally",
      "John, this is Sally",

      "Sally, this is Harry",
      "Sally, this is Sally",
      "Sally, this is John"]


  it "calculates length with list comprehension" $ do
    let xs = take 100 (repeat 'a')
    (len xs) `shouldBe` (length xs)


  it "calculates the largest palindrome given by the product of 2 3-digit numbers" $ do
    palindrome `shouldBe` (906609::Int)


  it "calculates the largest palindrome given by the product of 2 3-digit numbers, including base numbers" $ do
    palindromeBase `shouldBe` (906609,993,913)


palindrome :: Int
palindrome = maximum [n | x <- [111..999], y <- [111..999], let n = x * y, let s = show n, s == reverse s]

palindromeBase :: (Int, Int, Int)
palindromeBase =
  let xs = [(n, x, y) | x <- [111..999], y <- [111..999], let n = x * y, let s = show n, s == reverse s] in
    foldr (\(n, x, y) (gn, gx, gy) -> if n > gn then (n,x,y) else (gn, gx, gy)) (head xs) xs

len :: [a] -> Int
--len xs = foldl (\a i -> a + i) 0 [1 | _ <- xs]
len xs = foldl1 (+) [1 | _ <- xs]


fzbz :: Int -> String
fzbz x =
  case x of
  x' | (x' `mod` 2) == 0 && (x' `mod` 3) == 0  -> "fizzbuzz"
  x' | (x' `mod` 2) == 0                       -> "fizz"
  x' | (x' `mod` 3) == 0                       -> "buzz"
  _                                            -> show x

fzbz_foldl :: Int -> String
fzbz_foldl x =
  let mul = [(2, "fizz"), (3, "buzz")]
      r = foldl ff "" mul in
  if r == "" then (show x) else r
  where
    ff acc (d, f) = if (x `mod` d == 0) then acc ++ f else acc

buildList :: [(Int, Int)]
buildList = build' 1 1 []


build' :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
build' x y acc =
  case (x, y) of
  (10, 10) -> maybeAdd acc x y
  (_,  10) -> build'(x+1) (1)     ( maybeAdd acc x y)
  (_, _)   -> build' (x)   (y+1)   ( maybeAdd acc x y)
  where
    maybeAdd acc' x' y' =
      if y' `mod` 2 == 0
      then acc' ++ [(x'*2, y')]
      else acc'
