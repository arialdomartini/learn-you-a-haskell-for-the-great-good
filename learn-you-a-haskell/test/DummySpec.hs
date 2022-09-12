module DummySpec(spec) where

import Test.Hspec

spec :: Spec
spec = do

  it "42 is 42" $ do
    "42" `shouldBe` "42"
