module Functors.FunctorSpec where

import Test.Hspec

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' _ [] = []
  fmap' f (h:t) = f h : fmap' f t

spec :: Spec
spec = do
  it "maps on a list" $ do
    let l = [1,2,3] :: [Int]
        r = fmap' (*2) l
        in r `shouldBe` [2,4,6]
