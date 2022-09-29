module OwnTypesSpec where

import Test.Hspec
import Geometry.Shapes

spec :: Spec
spec = do
  -- it cannot use the type constructor, because it is not exported
  -- it "uses a custom type" $ do
  --    (area (Circle (Point 1 2) 3)) `shouldBe` 3 * 3 * pi
  --    (area (Rectangle (Point 0 8) (Point 9 0))) `shouldBe` 8 * 9

  -- it "partially applied type constructors" $ do
  --   let rays = [1,2,3,4] :: [Float]
  --       p0 = (Point 0 0)
  --       ctr = Circle p0
  --       expected = [(Circle p0 1), (Circle p0 2), (Circle p0 3), (Circle p0 4)] :: [Shape]
  --       in (fmap ctr rays) `shouldBe` expected

  it "creates circles from base + nudge" $ do
    ((nudge 5 0) . baseCircle $ 10) `shouldBe`  ((nudge 2.5 0) . (nudge 2.5 0) . baseCircle $ 10)
