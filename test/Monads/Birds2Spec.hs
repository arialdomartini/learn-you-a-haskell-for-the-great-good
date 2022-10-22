module Monads.Birds2Spec where

import Test.Hspec

type Birds = Int
type Left = Birds
type Right = Birds
data Pole = Pole Birds Birds deriving (Eq, Show)
data State = Balanced Pole | FallenDown deriving (Eq, Show)

instance Semigroup State where
  FallenDown <> _ = FallenDown
  _ <> FallenDown = FallenDown
  Balanced (Pole l r) <> Balanced (Pole l' r') =
    let totL = l + l'
        totR = r + r' in
      if abs (totL - totR) < 4
      then Balanced (Pole totL totR)
      else FallenDown

instance Monoid State where
  mempty = Balanced (Pole 0 0)
  mappend = (<>)

spec :: Spec
spec = do
  it "falls with 4 birds on the left" $ do
    foldMap Balanced [Pole 1 1, Pole 1 1, Pole 1 1, Pole 1 0, Pole 0 (-3)]
      `shouldBe` FallenDown
