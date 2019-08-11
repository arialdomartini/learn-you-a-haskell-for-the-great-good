module Chapter9.RandomnessSpec where

import Test.Hspec
import Chapter9.Randomness
import System.Random

spec = do
  it "tosses a coin 3 times" $ do
    threeCoins (mkStdGen 100) `shouldBe` (True, False, False)
