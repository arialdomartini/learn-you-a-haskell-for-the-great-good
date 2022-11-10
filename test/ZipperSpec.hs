{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module ZipperSpec where

import Test.Hspec

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

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

spec :: Spec
spec = do
  it "should modify W in X" $ do
    let Node root l r = freeTree
        Node value l' r' = r
        Node _ l'' r'' = l'

        nl' = Node 'X' l'' r''
        nr = Node value nl' r'
        nroot = Node root l nr in

      nroot `shouldBe`
       Node 'P'
        (Node 'O'
                (Node 'L' (Node 'N' Empty Empty)
                (Node 'T' Empty Empty))
        (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)))
        (Node 'L'
                (Node 'X' (Node 'C' Empty Empty)   -- <-- This W
                          (Node 'R' Empty Empty))
                 (Node 'A'
                          (Node 'A' Empty Empty)
                          (Node 'C' Empty Empty)))
