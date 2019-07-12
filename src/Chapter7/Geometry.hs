module Chapter7.Geometry (
  sphereVolume
)
where

sphereVolume :: Float -> Float
sphereVolume r = (4.0/3.0) * pi * (r ^ 3)
