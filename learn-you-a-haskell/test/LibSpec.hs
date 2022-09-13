module LibSpec(spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do

  it "doubles numbers" $ do
    (20::Int) `shouldBe` (double 10)

  it "concatenates lists" $ do
    [1,2,3] ++ [4,5,6] `shouldBe` ([1,2,3,4,5,6]::[Int])
