module Geometry
(
    sphereVolume
  , sphereArea
  , cubeVolume
  , cubeArea
  , cuboidVolume
  , cuboidArea
)


where

type Radius = Float
type Volume = Float
type Area = Float
type Edge = Float

sphereVolume :: Radius -> Volume
sphereVolume r = 4/3 * pi * r^ (3 :: Int)

sphereArea :: Radius -> Area
sphereArea r = 4 * pi * r ^ (2 :: Int)

cubeVolume :: Edge -> Volume
cubeVolume e = e ^ (3 :: Int)

cubeArea :: Edge -> Area
cubeArea e = 6 * e ^ (2 :: Int)

cuboidVolume :: Edge -> Edge -> Edge -> Volume
cuboidVolume x y z = rectangleArea x y * z

cuboidArea :: Edge -> Edge -> Edge -> Area
cuboidArea x y z = rectangleArea x y * 2 + rectangleArea x z * 2 + rectangleArea z y * 22 * (rectangleArea x y)

rectangleArea :: Edge -> Edge -> Area
rectangleArea x y = x * y
