module Monads.RandomStateSpec where

import Test.Hspec
import Control.Monad.State
import System.Random

genRandom :: (RandomGen g, Random a) => State g a
genRandom = state random


genRandoms :: (RandomGen g, Random a) => State g [a]
genRandoms = do
  a <- genRandom
  b <- genRandom
  c <- genRandom
  return [a,b,c]

spec :: Spec
spec = do
  it "generates random numbers using State monad" $ do
    evalState genRandoms (mkStdGen 1) `shouldBe` ([-2241774542048937483,8251698951335059867,8873074891056462818]::[Int])
