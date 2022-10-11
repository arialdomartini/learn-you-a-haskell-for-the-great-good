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

data Map' k v where
  Map' :: [(k, v)] -> Map' k v
  deriving (Show, Eq)

instance Functor' Maybe' where
  fmap' _ Nothing' = Nothing'
  fmap' f (Just' a) = Just' (f a)

instance Functor' Tree where
  fmap' _ Empty = Empty
  fmap' f (Node v l r) = Node (f v) (fmap' f l) (fmap' f r)

instance Functor' (Either' a) where
  fmap' _ (Left' l) = Left' l
  fmap' f (Right' r) = Right' (f r)

add' :: (k, v) -> Map' k v -> Map' k v
add' p (Map' l) = Map' (p : l)


foo :: IO String
foo = do
  return "Hey"


instance Functor' (Map' k) where
  fmap' _ (Map' []) = Map' []
  fmap' f (Map' ((k,v):t)) = (k, f v) `add'` fmap' f (Map' t)


instance Functor' IO where
  -- Linter suggests to replace this with
  -- f <$> m
  fmap' f m = do
    res <- m
    return $ f res

instance Functor' ((->)a) where
  fmap' f g = f . g



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

  it "Map' can be made an instance of Functor" $ do
    let map' = Map' [(1, "Joe"), (2, "Mary")]
        expected = Map' [(1, 3), (2, 4)] :: Map' Int Int
        in fmap' length map' `shouldBe` expected

  it "fmap a function to IO" $ do
    r2 <- fmap' length foo
    r2 `shouldBe` 3

  it "fmap a function to funtions" $ do
    let f i = i + 3 :: Int
      in (fmap' (*2) f) 5 `shouldBe` 16
