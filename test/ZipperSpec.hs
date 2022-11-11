{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ZipperSpec where

import Test.Hspec

data Tree a where
  Empty :: Tree a
  Node :: a -> Tree a -> Tree a -> Tree a
  deriving (Eq, Show)

freeTree :: Tree Char
freeTree =
  Node 'P'                                         -- Root
        (Node 'O'                                    -- Value
                (Node 'L' (Node 'N' Empty Empty)
                (Node 'T' Empty Empty))
        (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)))
        (Node 'L'                                    -- right
                (Node 'W' (Node 'C' Empty Empty)   -- <-- This W
                          (Node 'R' Empty Empty))
                 (Node 'A'
                          (Node 'A' Empty Empty)
                          (Node 'C' Empty Empty)))

expected :: Tree Char
expected =
  Node 'P'
        (Node 'O'
                (Node 'L' (Node 'N' Empty Empty)
                (Node 'T' Empty Empty))
        (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)))
        (Node 'L'
                (Node 'X' (Node 'C' Empty Empty)   -- <-- X here
                          (Node 'R' Empty Empty))
                 (Node 'A'
                          (Node 'A' Empty Empty)
                          (Node 'C' Empty Empty)))


simpleTree :: Tree String
simpleTree =
  Node "top" left right
  where left =  Node "left" Empty Empty
        right = Node "right" Empty Empty

data Direction = L | R deriving (Eq, Show)
type Directions = [Direction]



changeElement :: a -> Directions -> Tree a -> Tree a
changeElement v' [] (Node _ l r) = Node v' l r
changeElement v' (L:ds) (Node v l r) =
  Node v changed r
  where changed = changeElement v' ds l
changeElement v' (R:ds) (Node v l r) =
  Node v l changed
  where changed = changeElement v' ds r


-- With Breadcrumbs

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Eq, Show)
type Crumbs a = [ Crumb  a]

goLeft :: (Tree a, Crumbs a) -> (Tree a, Crumbs a)
goLeft (Empty, _) = error "You reached the end"
goLeft (Node v l r, bs) = (l, LeftCrumb v r:bs)


goRight :: (Tree a, Crumbs a) -> (Tree a, Crumbs a)
goRight (Empty, _) = error "You reached the end"
goRight (Node v l r, bs) = (r, RightCrumb v l:bs)

--
--     v
--   l   r

goUp :: (Tree a, Crumbs a) -> (Tree a, Crumbs a)
goUp (r, (RightCrumb v l):bs) = (Node v l r, bs)
goUp (l, (LeftCrumb v r):bs) = (Node v l r, bs)

(-:) :: a -> (a -> b) -> b
a -: f = f a

spec :: Spec
spec = do
  it "should modify W in X using Pattern Matching" $ do
    let Node root l r = freeTree
        Node value l' r' = r
        Node _ l'' r'' = l'

        nl' = Node 'X' l'' r''
        nr = Node value nl' r'
        nroot = Node root l nr in

      nroot `shouldBe` expected

  it "should modify W in X using directions" $ do
    changeElement 'X' [R,L] freeTree `shouldBe` expected

  it "should walk a tree and keep breadcrumbs" $ do
    ((simpleTree, []) -: goLeft) `shouldBe` (Node "left" Empty Empty, [LeftCrumb "top" (Node "right" Empty Empty)])

  it "should walk up and down a tree and keep breadcrumbs" $ do
    ((simpleTree, []) -: goLeft -: goLeft -: goUp) `shouldBe` (Node "left" Empty Empty, [LeftCrumb "top" (Node "right" Empty Empty)])
