module TypeParametersSpec where

import Test.Hspec

data Vector a = Vector a a a deriving (Show, Eq)
sum' :: (Num a) =>  Vector a -> Vector a -> Vector a
sum' (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

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
