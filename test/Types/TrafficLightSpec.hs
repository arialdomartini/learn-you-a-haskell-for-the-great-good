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


data Maybe' a = Nothing' | Just' a

instance (Eq m) => Eq (Maybe' m) where
  (==) (Just' l) (Just' r) = l == r
  (==) Nothing' Nothing' = True
  (==) Nothing' (Just' _) = False
  (==) (Just' _) Nothing' = False

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

  it "implements Eq on Maybe'" $ do
    let j = Just' "hey"
        x = Just' "joe"
        n = Nothing' :: (Maybe' String)
        in do j /= x `shouldBe` True
              j /= j `shouldBe` False
              j == n `shouldBe` False
              j == j `shouldBe` True
              n == n `shouldBe` True
