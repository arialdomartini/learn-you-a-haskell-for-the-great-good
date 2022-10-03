{-# LANGUAGE InstanceSigs #-}
module Types.TrafficLightSpec where

import Test.Hspec

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  (==) :: TrafficLight -> TrafficLight -> Bool
  Red == Red = True
  Yellow == Yellow  = True
  Green == Green = True
  _ == _ = False


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
