{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}
module Geometry.Shapes(baseCircle, nudge, Shape, Point) where
data Point where Point :: Float -> Float -> Point
  deriving (Show, Eq)
data Shape = Circle Point Float | Rectangle Point Point
               deriving (Show, Eq)

origin :: Point
origin = Point 0 0

baseCircle :: Float -> Shape
baseCircle = Circle origin

nudge :: Float -> Float -> Shape -> Shape
nudge x0 y0 (Circle (Point x y) r) = Circle (Point (x0 + x) (y0 + y)) r
nudge x0 y0 (Rectangle (Point x y) (Point x2 y2 )) = Rectangle (Point (x + x0) (y + y0)) (Point (x2 + x0) (y2 + y0))
