{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE InstanceSigs #-}
module Monads.ProbabilitySpec where

import Test.Hspec
import Data.Ratio

type Prob a = (a, Rational)
newtype Probs a = Probs {getProbabilities :: [Prob a]} deriving (Eq, Show)

instance Functor Probs where
  fmap f (Probs probs) = Probs (fmap (\(v,p) -> (f v, p)) probs)


spec :: Spec
spec = do
  it "is a functor" $ do
    fmap (*2) (Probs [(100, 1%5), (90, 3%5), (1, 2%5)]) `shouldBe` Probs  [(200, 1%5), (180, 3%5), (2, 2%5)]
