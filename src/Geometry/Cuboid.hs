module Geometry.Cuboid
(
  , volume
  , area
)

where

import Geometry.Sizes

volume :: Edge -> Edge -> Edge -> Volume
volume x y z = rectangleArea x y * z

area :: Edge -> Edge -> Edge -> Area
area x y z = rectangleArea x y * 2 + rectangleArea x z * 2 + rectangleArea z y * 22 * (rectangleArea x y)
