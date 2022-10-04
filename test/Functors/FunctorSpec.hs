{-# LANGUAGE GADTs #-}

module Functors.FunctorSpec where

import Test.Hspec
import Types.TreeSpec (Tree(Node), Tree(Empty))

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' _ [] = []
  fmap' f (h:t) = f h : fmap' f t

data Maybe' a = Nothing' | Just' a deriving (Show, Eq)

data Either' a b where
  Left' :: a -> Either' a b
  Right' :: b -> Either' a b
  deriving (Eq, Show)

instance Functor' Maybe' where
  fmap' _ Nothing' = Nothing'
  fmap' f (Just' a) = Just' (f a)

instance Functor' Tree where
  fmap' _ Empty = Empty
  fmap' f (Node v l r) = Node (f v) (fmap' f l) (fmap' f r)

instance Functor' (Either' a) where
  fmap' _ (Left' l) = Left' l
  fmap' f (Right' r) = Right' (f r)

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

  it "Either can be made an instance of Functor, Left case" $ do
    let l = length :: (String -> Int)
      in fmap' l  (Left' "error") `shouldBe` Left' "error"

  it "Either can be made an instance of Functor, Right case" $ do
    (fmap' length  (Right' "success") :: (Either' String Int)) `shouldBe` Right' 7
