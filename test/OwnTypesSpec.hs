module OwnTypesSpec where

import Test.Hspec

data Point = Point Float Float deriving (Show, Eq)
data Shape = Circle Point Float | Rectangle Point Point
               deriving (Show, Eq)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ (2:: Int)
area (Rectangle (Point x y) (Point x2 y2)) = abs (x2-x) * abs (y2 -y)

spec :: Spec
spec = do
  it "uses a custom type" $ do
     (area (Circle (Point 1 2) 3)) `shouldBe` 3 * 3 * pi
     (area (Rectangle (Point 0 8) (Point 9 0))) `shouldBe` 8 * 9

  it "partially applied type constructors" $ do
    let rays = [1,2,3,4] :: [Float]
        p0 = (Point 0 0)
        ctr = Circle p0
        expected = [(Circle p0 1), (Circle p0 2), (Circle p0 3), (Circle p0 4)] :: [Shape]
        in (fmap ctr rays) `shouldBe` expected
