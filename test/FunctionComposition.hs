{-# OPTIONS_GHC -Wno-type-defaults #-}
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
