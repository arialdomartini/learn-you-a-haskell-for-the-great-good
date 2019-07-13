module Chapter8.OwnTypesSpec
  where
import Test.Hspec

main = hspec spec

spec = do
  it "should pass" $ do
    surface (Rectangle 0 0 10 10) `shouldBe` 100
    surface (Triangle 10 5) `shouldBe` 25


data Shape = Triangle Float Float | Rectangle Float Float Float Float

surface :: Shape -> Float
surface (Triangle b h) = b * h / 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
