module Types.TreeSpec where

import Test.Hspec

data Tree a = Empty | Node a (Tree a) (Tree a)
deriving instance Show a => Show (Tree a)

-- add' :: Tree' a -> a -> Tree' a
-- add'

find' :: (Ord a) => Tree a -- ^
  -> a -- ^
  -> Bool
find' Empty _ = False
find' (Node a left right) v =
  case v of
    x | x > a -> find' right x
    x | x < a -> find' left x
    _ -> True

spec :: Spec
spec = do
  it "empty tree" $ do
    let tree = Empty in
      find' tree (42 :: Int) `shouldBe` False

  it "single node" $ do
    let tree = Node 42 Empty Empty in
      do find' tree (42 :: Integer) `shouldBe` True
         find' tree (-9) `shouldBe` False


  it "3 nodes" $ do
    let tree = Node 42 (Node 5 Empty Empty) (Node 100 Empty Empty) in
      do find' tree (42 :: Integer)  `shouldBe` True
         find' tree (-9)  `shouldBe` False
         find' tree  5  `shouldBe` True
         find' tree 100 `shouldBe` True
