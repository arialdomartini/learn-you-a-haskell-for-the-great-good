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


goLeft :: Zipper a -> Zipper a
goLeft (Empty, _) = error "You reached the end"
goLeft (Node v l r, bs) = (l, LeftCrumb v r:bs)


goRight :: Zipper a -> Zipper a
goRight (Empty, _) = error "You reached the end"
goRight (Node v l r, bs) = (r, RightCrumb v l:bs)

goUp :: Zipper a -> Zipper a
goUp (r, (RightCrumb v l):bs) = (Node v l r, bs)
goUp (l, (LeftCrumb v r):bs) = (Node v l r, bs)
goUp (_, []) = error "You cannot go over the top"

(-:) :: a -> (a -> b) -> b
a -: f = f a

spec :: Spec
spec = do
  it "should modify root" $ do
    modifyRoot (++"!") (simpleTree, []) `shouldBe`
        (Node "top!" (Node "left" Empty Empty) (Node "right" Empty Empty), [])

  it "should modify an arbitary node" $ do
    (simpleTree, []) -: goRight -: modifyRoot (++"!") -: goUp -: modifyRoot (++"!!!") `shouldBe`
        (Node "top!!!" (Node "left" Empty Empty) (Node "right!" Empty Empty),[])
