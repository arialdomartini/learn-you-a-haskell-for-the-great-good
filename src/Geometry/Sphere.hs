module Geometry.Sphere
(
    volume
  , area
)


where

import Geometry.Sizes

volume :: Radius -> Volume
volume r = 4/3 * pi * r^ (3 :: Int)

area :: Radius -> Area
area r = 4 * pi * r ^ (2 :: Int)
