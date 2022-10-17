{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module FoldableSpec where

import Test.Hspec
import qualified Data.Foldable as F

class Foldable' f where
  foldr' :: (a -> b -> b) -> b -> f a -> b
  foldMap :: (Monoid m) => (a -> m) -> f a -> m

instance Foldable' [] where
  foldr' _ a [] = a
  foldr' f a (h:t) = foldr' f (f h a) t

instance Foldable' Maybe where
  foldr' _ a Nothing = a
  foldr' f a (Just v) = f v a


data Tree a = Leaf | Node {value:: a, left:: Tree a, right:: Tree a}

instance Foldable' Tree where
  foldr' _ a Leaf = a
  foldr' f a (Node v l r) =
    let ar = foldr' f a r
        al = foldr' f ar l in
      f v al

class MapFoldable f where
  foldMap' :: (Monoid m) => (a -> m) -> f a -> m

newtype Sum a = Sum { getSum :: a } deriving (Eq, Show)

instance MapFoldable [] where
  foldMap' _ [] = mempty
  foldMap' f (x:xs) =
    let m = f x
        rest = foldMap' f xs in
      m <> rest

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend = (<>)


instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node v l r) = (f v) <> (F.foldMap f l) <> (F.foldMap f r)

gauss :: Fractional a => a -> a
gauss n = n * (n + 1) /2

spec :: Spec
spec = do
  it "List is an instance of Foldable" $ do
    foldr' (+) 0 [1..100] `shouldBe` gauss 100
    foldr' (*) 0 [1..50] `shouldBe` foldr (*) 0 [1..50]

  it "Maybe is an instance of Foldable" $ do
    foldr' (+) 7 (Just 5) `shouldBe` 12
    foldr' (+) 2 Nothing  `shouldBe` 2
    foldr' (+) 2 (Just 4) `shouldBe` 6

  it "folds a tree" $ do
    let tree = Node { value = 10, left = l   , right = r }
        r    = Node { value = 30, left = Leaf, right = Leaf }
        l    = Node { value = 50, left = Leaf, right = Leaf } in
      foldr' (+) 0 tree `shouldBe` 10 + 30 + 50

  it "sums with foldMap" $ do
    foldMap' Sum [1..100] `shouldBe` Sum 5050

  it "folds a tree with FoldMap" $ do
    let tree = Node { value = 10, left = l   , right = r }
        r    = Node { value = 30, left = Leaf, right = Leaf }
        l    = Node { value = 50, left = Leaf, right = Leaf } in
      do foldr (+) 0 tree `shouldBe` 10 + 30 + 50
         sum tree `shouldBe` 10 + 30 + 50
