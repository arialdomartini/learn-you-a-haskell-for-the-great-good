module PatternMatchingSpec where

import Test.Hspec

len :: [a] -> Int
len [] = 0
len (_ : t) = 1 + len t

firstLetter :: String -> String
firstLetter [] = error "Empty string!"
firstLetter s@(f:_) = "The first letter of " ++ s ++ " is " ++ [f]

buy :: Double -> String
buy expense
  | expense <= 100 = insult
  | expense <= 200 = anotherInsult
  | expense >= 1000 = "you are a spendthrift"
  | otherwise = error "Undefined"
  where insult = "you are Scrooge"
        anotherInsult = "ordinary. So boring"

initials :: String -> String -> String
initials name surname =
  [n] ++ "." ++ [s] ++ "."
  where
    (n:_) = name
    (s:_) = surname

spec :: Spec
spec = do
  it "implements length with recursion and pattern matching" $ do
    let xs = [1,2,3,4] :: [Int]
    len xs `shouldBe` length xs

  it "provides as-pattern" $ do
    (firstLetter "Hello") `shouldBe` "The first letter of Hello is H"


  it "uses guards" $ do
    (buy 100) `shouldBe` "you are Scrooge"
    (buy 200) `shouldBe` "ordinary. So boring"
    (buy 1000) `shouldBe` "you are a spendthrift"

  it "uses pattern matches in where clauses" $ do
    (initials "Pippo" "Franco") `shouldBe` "P.F."
