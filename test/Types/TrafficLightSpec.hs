{-# LANGUAGE InstanceSigs #-}
module Types.TrafficLightSpec where

import Test.Hspec

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  (==) :: TrafficLight -> TrafficLight -> Bool
  (==) l r =
    case (l,r) of
      (Red, Red) -> True
      (Yellow, Yellow) -> True
      (Green, Green) -> True
      (_,_) -> False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow  = "Yellow light"
    show Green = "Green light"

spec :: Spec
spec = do
  it "implements Eq" $ do
    let
    Red == Red `shouldBe` True
    Yellow == Yellow `shouldBe` True
    Green == Green `shouldBe` True

    Red == Yellow `shouldBe` False
    Red == Green `shouldBe` False
    Yellow == Red `shouldBe` False
    Yellow == Green `shouldBe` False
    Green == Red `shouldBe` False
    Green == Yellow `shouldBe` False

  it "implements Show" $ do
    show Red `shouldBe` "Red light"
    show Yellow `shouldBe` "Yellow light"
    show Green `shouldBe` "Green light"
