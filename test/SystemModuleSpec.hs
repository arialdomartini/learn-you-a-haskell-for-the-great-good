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
