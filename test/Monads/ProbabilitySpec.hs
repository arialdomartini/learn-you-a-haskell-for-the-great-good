{-# OPTIONS_GHC -Wno-type-defaults #-}
module Monads.ProbabilitySpec where

import Test.Hspec
import Data.Ratio

newtype Probability a = Probability {getProbability :: (a, Rational)} deriving (Eq, Show)

instance Functor Probability where
  fmap f prob =
    let (v, p) = getProbability prob in
      Probability (f v, p )

spec :: Spec
spec = do
  it "is a functor" $ do
    fmap (*2) (Probability (100, 1%5)) `shouldBe` Probability (200, 1%5)
