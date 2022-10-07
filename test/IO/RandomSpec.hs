module IO.RandomSpec where

import Test.Hspec
import System.Random (StdGen, Random (random))
import System.Random.Stateful (mkStdGen)

spec :: Spec
spec = do
  it "generates pseudo random numbers" $ do
    let (r1, g1) = random (mkStdGen 100) :: (Int, StdGen)
        (r2, _) = (random g1) :: (Int, StdGen)
      in do r1 `shouldBe` 9216477508314497915
            r2 `shouldBe` (-6917749724426303066)
