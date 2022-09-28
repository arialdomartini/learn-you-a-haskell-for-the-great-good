{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module SystemModuleSpec where

import qualified Data.Char   as Char
import           Data.List   (isPrefixOf, nub, sort, tails)
import           GHC.OldList (group)
import           Test.Hspec
import           Data.Char (digitToInt)
import qualified Data.Map as Map
import GHC.Unicode (isDigit)

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
      nub' []     = []
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
        find' ns ss@(_:st) =
          let len = length ns
              next = take len ss
          in  (next == ns) || find' ns st
        stack = [9, 9, 1,2 , 9, 9] :: [Int]
        in do find' [1,2] stack `shouldBe` True
              find' [9,2] stack `shouldBe` False

  it "another approach to tell if a list is contained in another one" $ do
    let
        find' :: Eq a => [a] -> [a] -> Bool
        find' n = any (isPrefixOf n) . tails
        stack = [9, 9, 1,2 , 9, 9] :: [Int]
        in do find' [1,2] stack `shouldBe` True
              find' [9,2] stack `shouldBe` False

  it "ciphers a string" $ do
    let
      cipher :: String -> String
      cipher s = [(Char.chr . (+1) . Char.ord) c | c <- s]
      in cipher "abcdef" `shouldBe` "bcdefg"

  it "uses foldl for summing 1_000_000 ones" $ do
    let
      upto = 1000000 :: Int
      result = foldl (+) 0 (replicate 1 upto)
      in result `shouldBe` upto

  it "finds the first number whose digits sums up to 40" $ do
    let
      sumDigits :: Int -> Int
      sumDigits d = sum (map digitToInt (show d ))
      in head ((dropWhile (\d -> sumDigits d /=40)) [1..]) `shouldBe` 49999

  it "find keys in association lists" $ do
    let
      findKey' :: Eq k => k -> [(k, a)] -> Maybe a
      findKey' _ [] = Nothing
      findKey' key ((k,a):r) = if key == k then Just a else findKey' key r
      assoc = [("foo", 42), ("bar", 28), ("baz", -9)] :: [(String, Int)]
      in do (findKey' "foo" assoc) `shouldBe` lookup "foo" assoc
            (findKey' "not-existing" assoc) `shouldBe` lookup "not-existing" assoc

  it "find keys in association lists, alternative implementation" $ do
    let
      findKey' :: Eq k => k -> [(k, a)] -> a
      findKey' key xs = snd . head . filter (\(k,_) -> k == key) $ xs
      assoc = [("foo", 42), ("bar", 28), ("baz", -9)] :: [(String, Int)]
      in (findKey' "foo" assoc) `shouldBe` 42

  it "implements lookup with foldr" $ do
    let
      lookup' :: Eq k => k -> [(k, a)] -> Maybe a
      lookup' key xs = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing xs
      assoc = [("foo", 42), ("bar", 28), ("baz", -9)] :: [(String, Int)]
      in do (lookup' "foo" assoc) `shouldBe` lookup "foo" assoc
            (lookup' "not-existing" assoc) `shouldBe` lookup "not-existing" assoc


  it "looks up in Maps" $ do
    let
        assoc = [("foo", 42), ("bar", 28), ("baz", -9)] :: [(String, Int)]
        map' = Map.fromList assoc
        in do Map.lookup "foo" map' `shouldBe` Just 42
              Map.lookup "not-existing" map' `shouldBe` Nothing

  -- Map k  a
  -- k -> a -> Map k a -> Map k a

  it "creates Maps using Map.inser" $ do
    let
      map' =
        (Map.insert "foo" 42 ) .
        (Map.insert "bar" 28) .
        Map.insert "baz" (-9) $
        Map.empty

      in do Map.lookup "foo" map' `shouldBe` ((Just 42) :: Maybe Int)
            Map.lookup "not-existing" map' `shouldBe` Nothing

  it "converts a string with phone number to list of numbers" $ do
    let
      phoneNumber = "0039-0573/28.0.21"
      convertToNumbers :: String -> [Int]
      convertToNumbers s = map digitToInt . filter isDigit $ s
      in convertToNumbers phoneNumber `shouldBe` [0,0,3,9,0,5,7,3,2,8,0,2,1]
