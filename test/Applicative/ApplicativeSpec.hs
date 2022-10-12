{-# OPTIONS_GHC -Wno-type-defaults #-}
module Applicative.ApplicativeSpec where

import Test.Hspec

class Applicative' f where
  (<**>) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe where
  Nothing <**> _ = Nothing
  -- more concisely:
  -- Just f <**> v = fmap f v
  Just f <**> Just v = Just (f v)
  Just _ <**> Nothing = Nothing

spec :: Spec
spec = do
  it "implements applicative for Maybe" $ do
    let m = Just 10 :: (Maybe Int)
        f = (*)
        a = fmap f m -- Just (10*)
        in a <**> Just 2 `shouldBe` Just 20

  it "implements applicative for Maybe" $ do
    ((*) <$> Just 10) <**> Just 2 `shouldBe` Just 20
    ((*) <$> Just 10) <**> Nothing `shouldBe` Nothing
    ((*) <$> Nothing) <**> Just 2 `shouldBe` Nothing
    ((*) <$> Nothing) <**> Nothing `shouldBe` Nothing

  it "can apply the internal function using fmap" $ do
    let a = fmap (*) (Just 10)
        in fmap (\f -> f 2) a `shouldBe` Just 20

  it "uses pure" $ do
     pure ("hey " ++) <*> pure "Joe"  `shouldBe` Just "hey Joe"
