module Functors.FunctorSpec where

import Test.Hspec

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' _ [] = []
  fmap' f (h:t) = f h : fmap' f t

data Maybe' a = Nothing' | Just' a deriving (Show, Eq)

instance Functor' Maybe' where
  fmap' _ Nothing' = Nothing'
  fmap' f (Just' a) = Just' (f a)

spec :: Spec
spec = do
  it "maps on a list" $ do
    let l = [1,2,3] :: [Int]
        r = fmap' (*2) l
        in r `shouldBe` [2,4,6]

  it "Maybe is an instance of Functor" $ do
    fmap' (++ "!") (Just' "hey") `shouldBe` Just' "hey!"
