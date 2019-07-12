module Chapter7.GeometrySpec where

import Chapter7.Geometry
import Test.Hspec

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  it "calculates the volume of a sphere" $ do
    sphereVolume 2.0 `shouldBe` 33.510323
