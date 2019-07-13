module Chapter8.OwnTypesSpec
  where
import Test.Hspec
import Data.List

main = hspec spec

spec = do
  it "should pass" $ do
    surface (Rectangle (Point 0 0) (Point 10 10)) `shouldBe` 100
    surface (Triangle 10 5) `shouldBe` 25

  it "partially applies a data constructor" $ do
    (surface getLast) `shouldBe` 25

-- Data constructors are functions, so they can be partially applied
getLast = last $ map (Triangle 10) [1,2,3,4,5]

type Base = Float
type Height = Float
data Point = Point Float Float deriving Show
data Shape = Triangle Base Height | Rectangle Point Point deriving Show


-- Shape is a Type Constructor
-- Triangle and Rectangle are Data Constructors
-- :t Triangle
-- Triangle :: Float -> Float -> Shape
-- :t Rectangle
-- Rectangle :: Float -> Float -> Float -> Float -> Shape
-- :t Shape
-- error: Data constructor not in scope: Shape

surface :: Shape -> Float
surface (Triangle b h) = b * h / 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

