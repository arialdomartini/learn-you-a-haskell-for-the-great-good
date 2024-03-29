{-# OPTIONS_GHC -Wno-type-defaults #-}
module Monads.WriterUsingStandardSpec where

import Test.Hspec
import Control.Monad.Writer
import Prelude hiding (gcd)
import Data.List (intercalate)
import DifferenceListSpec(DiffList, fromDiffList, toDiffList)

f :: Writer [String] Int
f =
  return "Hey" >>=
  \s -> writer (s ++ "!", ["screaming"]) >>=
  \s' -> writer (length s', ["calculating length"])

f' :: Writer [String] Int
f' = do
  let s = "Hey"
  s' <- writer (s ++ "!", ["screaming"])
  writer (length s', ["calculating length"])

type Logs = [String]

logNumber :: Int -> Writer Logs Int
logNumber n = writer (n, ["got " ++ show n])

doMult :: Int -> Int -> Writer Logs Int
doMult a b = do
  x <- logNumber a
  y <- logNumber b
  tell ["multiplying " ++ show a ++ " and " ++ show b]
  let result = x * y
  tell ["got " ++ show result]
  return (x * y)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd n m = gcd m (n `mod` m)


gcd' :: Int -> Int -> Writer [String] Int
gcd' n 0 = do
  tell ["Finished with " <> show n]
  return n
gcd' n m = do
  tell [show n ++ " `mod` " ++ show m]
  gcd' m (n `mod` m)


gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' n 0 = do
  tell $ toDiffList ["Finished with " <> show n]
  return n
gcd'' n m = do
  tell $ toDiffList [show n ++ " `mod` " ++ show m]
  gcd'' m (n `mod` m)


spec :: Spec
spec = do
  it "bind monadic functions" $ do
    let (result, logs) = runWriter f
      in do logs `shouldBe` ["screaming", "calculating length"]
            result `shouldBe` 4

  it "bind monadic functions, with do notation" $ do
    let (result, logs) = runWriter f'
      in do logs `shouldBe` ["screaming", "calculating length"]
            result `shouldBe` 4

  it "uses default values" $ do
    runWriter (return 3 :: Writer String Int) `shouldBe` (3, "")

  it "uses a different default value" $ do
    runWriter (return 3 :: Writer (Sum Int) Int) `shouldBe` (3, Sum {getSum = 0})

  it "multiplies numbers with log" $ do
    runWriter (doMult 2 3) `shouldBe` (6, ["got 2", "got 3", "multiplying 2 and 3", "got 6"])

  it "calculates the greatest common divisor" $ do
    gcd 8 4 `shouldBe` 4
    gcd 12 5 `shouldBe` 1
    gcd 100 25 `shouldBe` 25
    gcd 100 15 `shouldBe` 5

  it "calculates the greatest common divisor, tracking the steps" $ do
    fst (runWriter (gcd' 8 4)) `shouldBe` 4
    intercalate ", " (snd (runWriter (gcd' 12 5))) `shouldBe` "12 `mod` 5, 5 `mod` 2, 2 `mod` 1, Finished with 1"


  it "calculates the greates common divisor tracking steps with DiffLists" $ do
    let result = gcd'' 12 5
        (_, l) = runWriter result
        logs = fromDiffList l in
      logs `shouldBe` ["12 `mod` 5", "5 `mod` 2", "2 `mod` 1", "Finished with 1"]
