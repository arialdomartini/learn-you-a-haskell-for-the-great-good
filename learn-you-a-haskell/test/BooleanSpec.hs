module BooleanSpec(spec) where

import Test.Hspec

spec :: Spec
spec = do
  it "negates booleans" $ do
    not (True && False) `shouldBe` (False || True)
