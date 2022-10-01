module TypeParametersSpec where

import Test.Hspec

data Vector a = Vector a a a deriving (Show, Eq)

sum' :: (Num a) =>  Vector a -> Vector a -> Vector a
sum' (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

dotProd' :: (Num a) => Vector a -> Vector a-> a
dotProd' (Vector x1 y1 z1) (Vector x2 y2 z2) = (x1*x2) + (y1*y2) + (z1*z2)

vecProd' :: (Num a) => Vector a -> Vector a-> Vector a
vecProd' (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1*x2) (y1*y2) (z1*z2)

-- deriving Show is requested by `shouldBe`
data Person = Person {firstName:: String, secondName:: String, age:: Int} deriving (Eq, Show, Read)

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Bounded, Show, Eq, Enum, Ord)

spec :: Spec
spec = do
  it "uses a vector" $ do
    let v = Vector 1 2 3 :: Vector Int
        Vector x y z = v
         in do x `shouldBe` 1
               y `shouldBe` 2
               z `shouldBe` 3

  it "sums vectors" $ do
    let v = Vector 1 1 1 :: Vector Int
        w = Vector 2 0 2 :: Vector Int
        in (v `sum'` w) `shouldBe` Vector 3 1 3

  it "dotprod vectors" $ do
    let v = Vector 1 1 1 :: Vector Int
        w = Vector 2 0 2 :: Vector Int
        in (v `dotProd'` w) `shouldBe` 1*2 + 1*0 + 1*2

  it "vectorial product" $ do
    let v = Vector 1 1 1 :: Vector Int
        w = Vector 2 0 2 :: Vector Int
        in (v `vecProd'` w) `shouldBe` Vector (1*2) (1*0) (1*2)

  it "compares Eq instances" $ do
    let person1 = Person {firstName="John", secondName="Doe", age=66} :: Person
        person2 = Person {firstName="John", secondName="Doe", age=66} :: Person  in
      person1 `shouldBe` person2

  it "can build a record with Read from its Show output" $ do
    let person = Person {firstName="John", secondName="Doe", age=66} :: Person in
      (read . show) person `shouldBe` person

  it "uses Bounded, and compares using Eq" $ do
    (minBound :: Days) `shouldBe` Monday
    (maxBound :: Days) `shouldBe` Sunday

  it "uses Ord with syntactic order" $ do
    Monday < Tuesday `shouldBe` True
    Monday `compare` Tuesday `shouldBe` LT

  it "uses Enum" $ do
    succ Monday `shouldBe` Tuesday


  it "generates full list combining Bounded, Enum (Ord is not needed)" $ do
    ([minBound .. maxBound] :: [Days]) `shouldBe` [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
