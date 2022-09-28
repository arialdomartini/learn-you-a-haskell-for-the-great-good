module OwnTypesSpec where

import Test.Hspec

data Shape = Circle Float Float Float | Rectangle Float Float
               deriving (Show, Eq)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ (2:: Int)
area (Rectangle b h) = b * h

spec :: Spec
spec = do
  it "uses a custom type" $ do
     (area (Circle 1 2 3)) `shouldBe` 3 * 3 * pi
     (area (Rectangle 8 9)) `shouldBe` 8 * 9

  it "partially applied type constructors" $ do
    let rays = [1,2,3,4] :: [Float]
        ctr = Circle 0 0
        expected = [(Circle 0 0 1), (Circle 0 0 2), (Circle 0 0 3), (Circle 0 0 4)] :: [Shape]
        in (fmap ctr rays) `shouldBe` expected
