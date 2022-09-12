module DummySpec(spec) where

import Test.Hspec

spec :: Spec
spec = do

  it "" $ do
    "42" `shouldBe` "42"
