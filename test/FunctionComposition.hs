{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module FunctionComposition where

import Test.Hspec


(/.) :: (b -> c) -> (a -> b) -> (a -> c)
(/.) f g n = f (g n)

spec :: Spec
spec = do
  it "composes functions" $ do
    let
      f = (* 2)
      g = (+ 12) in
      (f /. g) 12 `shouldBe` f ( g 12 )


  it "returns all negative numbers with function composition" $ do
    let allNegative xs = fmap (negate . abs) xs
      in allNegative [1, -2, 3, -4] `shouldBe` ([-1,-2,-3,-4] :: [Int])

  it "composes functions with multiple parameters" $ do
    let
      withFuncInvocation :: Int
      withFuncInvocation = sum (replicate 5 (max 6 8))

      withFuncComposition :: Int
      withFuncComposition = (sum . replicate 5) $ max 6 8

      in withFuncInvocation `shouldBe` withFuncComposition

  it "another example of composition with multiple parameters" $ do
    let
      invocation :: [Int]
      invocation = replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))

      composition :: [Int]
      composition = replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]
      in invocation `shouldBe` composition

  it "can use point free style" $ do
    let
      sum' :: Num a => [a] -> a
      sum' = foldl1 (+)
      in sum' [1,2,3] `shouldBe` 1 + 2 + 3

  it "another example of point free style" $ do
    let
      fn x = ceiling (negate (tan (cos (max 50 x))))
      fnPointFree x = ceiling . negate . tan . cos $ max 50 x
      in fn 100 `shouldBe` fnPointFree 100


  it "3rd example of point-free style" $ do
    let
      oddSquareSum :: Integer
      oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

      fnPointFree :: Integer
      fnPointFree = sum . (takeWhile (<10000)). (filter  odd) $ map (^2) [1..]
      in oddSquareSum `shouldBe` fnPointFree
