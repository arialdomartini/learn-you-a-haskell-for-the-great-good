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
