{-# LANGUAGE GADTs #-}
module ZipperSpec where

import Test.Hspec

data Tree a where
  Empty :: Tree a
  Node :: a -> Tree a -> Tree a -> Tree a
  deriving (Eq, Show)

data Breadcrumb a where
  LeftCrumb :: a -> Tree a -> Breadcrumb a
  RightCrumb :: a -> Tree a -> Breadcrumb a
  deriving (Eq, Show)

type Breadcrumbs a = [ Breadcrumb a ]

type Zipper a = (Tree a, Breadcrumbs a)

simpleTree :: Tree String
simpleTree =
  Node "top" left right
  where left =  Node "left" Empty Empty
        right = Node "right" Empty Empty

modifyRoot :: (a -> a) -> Zipper a -> Zipper a
modifyRoot _ (Empty, bs) = (Empty, bs)
modifyRoot f (Node v l r, bs) = (Node (f v) l r, bs)

spec :: Spec
spec = do
  it "should modify root" $ do
    modifyRoot (++"!") (simpleTree, []) `shouldBe`
        (Node "top!" (Node "left" Empty Empty) (Node "right" Empty Empty), [])
