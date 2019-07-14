module Chapter8.BinaryTreeSpec where
import Test.Hspec

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

main = hspec spec
spec = do
  it "builds a singleton tree" $ do
    singleton 2 `shouldBe` (Node 2 Empty Empty)

  it "should insert values in a tree" $ do
    insert  3( insert 5 (insert 20 (singleton 10))) `shouldBe`
      (Node 10 (Node 5 (Node 3 Empty Empty) Empty) (Node 20 Empty Empty))

singleton :: a -> Tree a
singleton x = Node x Empty Empty

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Empty = singleton a
insert a (Node x left right) = if a > x
  then Node x left (insert a right)
  else Node x (insert a left) right
