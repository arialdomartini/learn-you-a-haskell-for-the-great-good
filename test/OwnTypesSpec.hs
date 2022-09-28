module OwnTypesSpec where

import Test.Hspec

data Shape = Circle Float Float Float | Rectangle Float Float
               deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ (2:: Int)
area (Rectangle b h) = b * h

spec :: Spec
spec = do
  it "uses a custom type" $ do
     (area (Circle 1 2 3)) `shouldBe` 3 * 3 * pi
     (area (Rectangle 8 9)) `shouldBe` 8 * 9
