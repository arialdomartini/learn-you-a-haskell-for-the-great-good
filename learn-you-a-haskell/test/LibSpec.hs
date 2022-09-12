module LibSpec(spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do

  it "doubles numbers" $ do
    (20::Int) `shouldBe` (double 10)
