module Chapter8.FunctorSpec where
import Test.Hspec
import qualified Chapter8.BinaryTreeSpec as Tree
  
main = hspec
spec = do
  it "List should be a functor" $ do
    fmap' (*2) [1,2,3] `shouldBe` [2,4,6]

  it "Maybe should be a functor" $ do
    fmap' (+10) (Just 8) `shouldBe` Just 18
    fmap' (+10) Nothing `shouldBe` Nothing

  it "Tree should be a functor" $ do
    let tree = Tree.insert 3 ( Tree.insert 5 (Tree.insert 20 (Tree.singleton 10)))
        expectedTree = Tree.insert 6 ( Tree.insert 10 (Tree.insert 40 (Tree.singleton 20)))
      in fmap' (*2) tree `shouldBe` expectedTree


class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' g f = map g f

instance Functor' Maybe where
  fmap' _ Nothing = Nothing
  fmap' g (Just x) = Just (g x)

instance Functor' Tree.Tree where
  fmap' _ Tree.Empty = Tree.Empty
  fmap' f (Tree.Node a l r) = Tree.Node (f a) (fmap' f l) (fmap' f r) 
