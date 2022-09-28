module GeometrySpec where

import Test.Hspec
import Geometry.Cube as Cube

spec :: Spec
spec = do
  it "should use function in other module" $ do
    Cube.volume 2 `shouldBe` 8
