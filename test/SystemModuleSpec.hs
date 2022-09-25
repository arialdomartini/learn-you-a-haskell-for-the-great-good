{-# OPTIONS_GHC -Wno-unused-matches #-}
module SystemModuleSpec where

import Test.Hspec
import Data.List (nub)

spec :: Spec
spec = do
  it "removes duplicates" $ do
    let
      cs = ['a', 'b', 'c', 'a', 'c']

      deduped x = filter (/= x)
      nub' :: Eq a => [a] -> [a]
      nub' [] = []
      nub' (x:xs) = x : nub'  (deduped x xs)

      in nub' cs `shouldBe` nub cs

  it "counts words" $ do
    let
      words'' :: [String] -> [Char] -> [String]
      words'' acc [] = acc
      words'' [] (x:xs) =
        if x == ' '
        then words'' [] xs
        else words'' [[x]] xs

      words'' acc@(a:as) (x:xs) =
        if x == ' '
        then words'' ([] : acc) xs
        else words'' ((a ++ [x]) : as) xs

      words' :: [Char] -> [String]
      words' = reverse . words'' [[]]

      sentence = "nel mezzo del cammin di nostra vita"
      in words' sentence `shouldBe` words sentence
