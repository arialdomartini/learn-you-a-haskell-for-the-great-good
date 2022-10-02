module Types.TreeSpec where

import Test.Hspec

data Tree a = Empty | Node a (Tree a) (Tree a)
deriving instance Show a => Show (Tree a)
deriving instance Eq a => Eq (Tree a)

-- add' :: Tree' a -> a -> Tree' a
-- add'

find' :: (Ord a) => Tree a -- ^
  -> a -- ^
  -> Bool
find' Empty _ = False
find' (Node a left right) v
    | v > a  = find' right v
    | v < a  = find' left v
    | v == a = True

singleton :: a -> Tree a
singleton v = Node v Empty Empty

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty a = singleton a
insert node@(Node v left right) a
    | a == v = node
    | a < v  = Node v (insert left a) right
    | a > v  = Node v left (insert right a)

spec :: Spec
spec = do
  it "empty tree" $ do
    let tree = Empty in
      find' tree (42 :: Int) `shouldBe` False

  it "single node" $ do
    let tree = Node 42 Empty Empty in
      do find' tree (42 :: Int) `shouldBe` True
         find' tree (-9) `shouldBe` False

  it "3 nodes" $ do
    let tree = Node 42 (Node 5 Empty Empty) (Node 100 Empty Empty) in
      do find' tree (42 :: Int)  `shouldBe` True
         find' tree (-9)  `shouldBe` False
         find' tree  5  `shouldBe` True
         find' tree 100 `shouldBe` True

  it "inserts a value in an empty tree" $ do
    let t = insert Empty (42 :: Int) in
      t `shouldBe` (Node 42 Empty Empty)

  it "inserts a value on the left branch of a singleton tree" $ do
    let t = insert (Node (100 :: Int) Empty Empty) 42 in
      t `shouldBe` Node (100 :: Int) (Node 42 Empty Empty) Empty

  it "inserts a value on the right branch of a singleton tree" $ do
    let t = insert (Node (100 :: Int) Empty Empty) 200 in
      t `shouldBe` Node (100 :: Int) Empty (Node 200 Empty Empty)

  it "does not change tree if value already exists singleton tree" $ do
    let orig = Node (100 :: Int) Empty Empty
        t = insert orig 100 in
        t `shouldBe` orig

  it "inserts a value on the left branch of a tree" $ do
    let t = insert (Node (100 :: Int) (Node 90 Empty Empty) Empty) 80 in
      t `shouldBe` Node (100 :: Int) (Node 90 (Node 80 Empty Empty) Empty) Empty
