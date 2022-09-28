module Geometry.Cube
(
    volume
  , area
)


where

import Geometry.Sizes

volume :: Edge -> Volume
volume e = e ^ (3 :: Int)

area :: Edge -> Area
area e = 6 * e ^ (2 :: Int)
