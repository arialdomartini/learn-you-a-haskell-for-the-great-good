module Chapter9.RandomnessSpec where

import Test.Hspec
import Chapter9.Randomness
import System.Random

spec = do
  it "tosses a coin 3 times" $ do
    threeCoins (mkStdGen 100) `shouldBe` (True, False, False)

  it "tosses a coins infinite times" $ do
    take 7 (infiniteCoins  (mkStdGen 100)) `shouldBe` [True, False, False, False, False, True, True]

  it "infinite coins with a custom implementation of randoms" $ do
    take 7 (infiniteCustomCoins  (mkStdGen 100)) `shouldBe` [True, False, False, False, False, True, True]

  it "generates random numbers in a range" $ do
    take 6 (getRandomRange (0, 6) (mkStdGen 2)) `shouldBe` [0, 3, 1, 2, 0, 5]
