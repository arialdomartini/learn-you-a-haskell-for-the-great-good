{-# OPTIONS_GHC -Wno-unused-matches #-}
module SystemModuleSpec where

import Test.Hspec
import Data.List (nub)
import GHC.OldList (group)
import Data.List (sort)

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' xs@(x:_) =
  let (e, ne) = span (==x) xs
  in e : group' ne


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

words' :: String -> [String]
words' = reverse . words'' [[]]


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
      sentence = "nel mezzo del cammin di nostra vita"
      in words' sentence `shouldBe` words sentence

  it "groups elements in a list" $ do
    let
      ns = [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7, 1, 1, 1] :: [Int]
      in group ns `shouldBe` group' ns

  it "count word occurrences" $ do
    let
      countWords' :: String -> [(String, Int)]
      countWords' = fmap (\g -> (head g, length g)) . group' . sort . words'
      in countWords' "ciao mamma come stai mamma ciao ciao" `shouldBe` [("ciao", 3 :: Int), ("come", 1), ("mamma", 2), ("stai", 1)]

  it "tells if a list is contained in another one" $ do
    let
        find' :: Eq a => [a] -> [a] -> Bool
        find' _ [] = False
        find' ns ss@(s:st) =
          let len = length ns
              next = take len ss
          in  (next == ns) || find' ns st
        stack = [9, 9, 1,2 , 9, 9] :: [Int]
        in do find' [1,2] stack `shouldBe` True
              find' [9,2] stack `shouldBe` False
