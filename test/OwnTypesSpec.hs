module OwnTypesSpec where

import Test.Hspec

data Point = Point Float Float deriving (Show, Eq)
data Shape = Circle Point Float | Rectangle Point Point
               deriving (Show, Eq)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ (2:: Int)
area (Rectangle (Point x y) (Point x2 y2)) = abs (x2-x) * abs (y2 -y)

origin :: Point
origin = Point 0 0

baseCircle :: Float -> Shape
baseCircle r = Circle origin r

nudge :: Float -> Float -> Shape -> Shape
nudge x0 y0 (Circle (Point x y) r) = Circle (Point (x0 + x) (y0 + y)) r
nudge x0 y0 (Rectangle (Point x y) (Point x2 y2 )) = Rectangle (Point (x + x0) (y + y0)) (Point (x2 + x0) (y2 + y0))


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

  it "creates circles from base + nudge" $ do
    ((nudge 5 0) . baseCircle $ 10) `shouldBe`  Circle (Point 5 0) 10
