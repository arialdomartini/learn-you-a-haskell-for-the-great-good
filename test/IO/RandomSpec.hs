module IO.RandomSpec where

import Test.Hspec
import System.Random (StdGen, Random (random))
import System.Random.Stateful (mkStdGen)

spec :: Spec
spec = do
  it "generates pseudo random numbers" $ do
    let (r, _) = random (mkStdGen 100) :: (Int, StdGen)
    r `shouldBe` 9216477508314497915
