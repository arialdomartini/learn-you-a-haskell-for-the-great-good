{-# OPTIONS_GHC -Wno-type-defaults #-}
module Applicative.ApplicativeSpec where

import Test.Hspec

class Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe where
  pure' = Just
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
     pure ("hey " ++) <*> pure "Joe"  `shouldBe` (pure "hey Joe" :: Maybe String)


  it "lifts a function" $ do
    let f a b c = (a,b,c) in
      pure f <*> [1,2,3] <*> ["a", "b", "c"] <*> ['x', 'y'] `shouldBe` [(1,"a",'x'), (1,"a",'y'), (1,"b",'x'), (1,"b",'y') ,(1,"c",'x'), (1,"c",'y')
                                                                       ,(2,"a",'x'), (2,"a",'y'), (2,"b",'x'), (2,"b",'y'), (2,"c",'x'), (2,"c",'y')
                                                                       ,(3,"a",'x'), (3,"a",'y'), (3,"b",'x'), (3,"b",'y'), (3,"c",'x'), (3,"c",'y')]

  it "shows that fmap and <$> are synomyms" $ do
    let f a b c = (a,b,c) in
      fmap f     [1,2,3] <*> ["a", "b", "c"] <*> ['x', 'y'] `shouldBe`
           f <$> [1,2,3] <*> ["a", "b", "c"] <*> ['x', 'y']
