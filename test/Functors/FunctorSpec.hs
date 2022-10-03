module Functors.FunctorSpec where

import Test.Hspec
import Types.TreeSpec (Tree(Node), Tree(Empty))

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' _ [] = []
  fmap' f (h:t) = f h : fmap' f t

data Maybe' a = Nothing' | Just' a deriving (Show, Eq)

instance Functor' Maybe' where
  fmap' _ Nothing' = Nothing'
  fmap' f (Just' a) = Just' (f a)

instance Functor' Tree where
  fmap' _ Empty = Empty
  fmap' f (Node v l r) = Node (f v) (fmap' f l) (fmap' f r)

spec :: Spec
spec = do
  it "maps on a list" $ do
    let l = [1,2,3] :: [Int]
        r = fmap' (*2) l
        in r `shouldBe` [2,4,6]

  it "Maybe is an instance of Functor" $ do
    fmap' (++ "!") (Just' "hey") `shouldBe` Just' "hey!"

  it "Tree can be made an instance of Functor" $ do
    let tree = Node (100 :: Int) (Node 90 (Node 80 Empty Empty) Empty) Empty
        expected = Node (200 :: Int) (Node 180 (Node 160 Empty Empty) Empty) Empty
    fmap' (*2) tree `shouldBe` expected
