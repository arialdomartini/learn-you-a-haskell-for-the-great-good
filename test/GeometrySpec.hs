module GeometrySpec where

import Test.Hspec
import Geometry

spec :: Spec
spec = do
  it "should use function in other module" $ do
    cubeVolume 2 `shouldBe` 8
