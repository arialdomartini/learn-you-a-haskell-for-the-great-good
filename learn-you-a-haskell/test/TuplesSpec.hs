module TuplesSpec where

import Test.Hspec

spec :: Spec
spec = do
  it "composes and decomposes tuples" $ do
    let t = ("foo", 42) :: (String, Int)
    (fst t, snd t) `shouldBe` t

  it "builds tuples using zip" $ do
    let f = \a b -> head $ zip [a] [b]
    (f "foo" 42) `shouldBe` (("foo", 42) :: (String, Int))


  it "tuple of bounded values is bounded" $ do
    let m = maxBound :: (Bool, Int, Char)
        n = (maxBound, maxBound, maxBound) :: (Bool, Int, Char)
    n `shouldBe` m
