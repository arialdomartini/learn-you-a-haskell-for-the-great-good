module Chapter8.OwnTypesSpec
  where
import Test.Hspec
import Data.List
import Data.Map as Map

main = hspec spec

spec = do
  it "should pass" $ do
    surface (Rectangle (Point 0 0) (Point 10 10)) `shouldBe` 100
    surface (Triangle 10 5) `shouldBe` 25

  it "partially applies a data constructor" $ do
    (surface getLast) `shouldBe` 25

  it "should use pattern matching with the record syntax" $ do
    surface' Triangle {base = 10, height = 10 } `shouldBe` 50


  it "Maybe, negative case" $ do
    divide 3.0 0.0 `shouldBe` Nothing'

  it "Maybe, positive case" $ do
    divide 6.0 2.0 `shouldBe` Just' 3.0

  it "sum of vectors" $ do
    (Vector 1 1 1) +! (Vector 2 2 2) `shouldBe` (Vector 3 3 3)

  it "vectorial multiplication of vector" $ do
    6 *! (Vector 1 1 1) `shouldBe` (Vector 6 6 6)

  it "scalar multiplication of vector" $ do
    (Vector 1 2 3) *^ (Vector 3 4 5) `shouldBe` (Vector 3 8 15)

  it "can compare laptops" $ do
    (Laptop I7 1000) > (Laptop I5 2000) `shouldBe` True

  it "should move a shape" $ do
    (move point before) `shouldBe` after
      where before = Rectangle (Point 0 0) (Point 10 10)
            point =  Point 5 3
            after =  Rectangle (Point 5 3) (Point 15 13)

-- Data constructors are functions, so they can be partially applied
getLast = last $ Data.List.map (Triangle 10) [1,2,3,4,5]

type Base = Float
type Height = Float
data Point = Point Float Float deriving Show
data Shape = Triangle {base :: Base, height :: Height } | Rectangle { topLeft :: Point, bottomRight :: Point } deriving Show


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
surface (Rectangle p1 p2) = base * height
  where base = distX p1 p2
        height = distY p1 p2

surface' Triangle {height = h, base = b} = b * h /2


distX (Point x1 _) (Point x2 _) = x2 -! x1
distY (Point _ y1) (Point _ y2) = y2 -! y1
(-!) a b = abs $ b - a


move :: Point -> Shape -> Shape
move (Point x y) (Rectangle (Point x1 y1) (Point x2 y2)) = Rectangle (Point (x1 +x) (y1+y)) (Point (x2 +x) (y2+y))


instance Eq Shape where
  (Rectangle (Point x1a y1a) (Point x2a y2a)) == (Rectangle (Point x1b y1b) (Point x2b y2b)) =
    (x1a == x1b) && (y1a == y1b) && (x2a == x2a) && (y2a == y2b)



divide :: (Fractional a, Eq a) => a -> a -> Maybe' a
divide a b = if b == 0 then Nothing' else Just' (a / b)

data Maybe' a = Nothing' | Just' a  deriving (Show, Eq)


data Vector a  = Vector a a a deriving (Show,Eq)

(+!) :: (Num a) => Vector a -> Vector a -> Vector a
(+!) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

(*!) :: (Num a) => a -> Vector a -> Vector a
(*!) n (Vector x y z) = Vector (x*n) (y*n) (z*n)

(*^) :: (Num a) => Vector a -> Vector a -> Vector a
(*^) (Vector x y z) (Vector a b c) = Vector (x*a) (y*b) (z*c)



data CPU = I5 | I6 | I7 deriving (Eq,Show)
data Laptop = Laptop CPU Int deriving Eq

-- the following could be replaced by simply deriving CPU from Ord
-- data CPU = I5 | I6 | I7 deriving (Eq,Show,Ord)
instance Ord CPU where
  compare a b = compare aa bb
    where aa = find a
          bb = find b
          find y = snd $ head $ Data.List.filter (\x -> fst x==y) x
          x = [ (I5, 1),  (I6, 2),  (I7, 3)]

instance Ord Laptop where
  compare (Laptop cpu1 frequency1) (Laptop cpu2 frequency2) = if cpu1 == cpu2 then compare frequency1 frequency2 else compare cpu1 cpu2
  
