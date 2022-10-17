{-# OPTIONS_GHC -Wno-type-defaults #-}
module FoldableSpec where

import Test.Hspec

class Foldable' f where
  foldr' :: (a -> b -> b) -> b -> f a -> b

instance Foldable' [] where
  foldr' _ a [] = a
  foldr' f a (h:t) = foldr' f (f h a) t


gauss :: Fractional a => a -> a
gauss n = n * (n + 1) /2

spec :: Spec
spec = do
  it "List can be made an instance of Foldable" $ do
    foldr' (+) 0 [1..100] `shouldBe` gauss 100
    foldr' (*) 0 [1..50] `shouldBe` foldr (*) 0 [1..50]
