module Geometry.Rectangle
(
    area
)


where

using Geometry.Sizes

area :: Edge -> Edge -> Area
area x y = x * y
