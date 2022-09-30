module TypeParametersSpec where

import Test.Hspec

data Vector a = Vector a a a deriving (Show)

spec :: Spec
spec = do
  it "uses a vector" $ do
    let v = Vector 1 2 3 :: Vector Int
        Vector x y z = v
         in do x `shouldBe` 1
               y `shouldBe` 2
               z `shouldBe` 3
