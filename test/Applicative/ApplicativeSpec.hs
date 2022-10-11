module Applicative.ApplicativeSpec where

import Test.Hspec

spec :: Spec
spec = do
  it "should pass" $ do
    "friends" `shouldBe` "friends"
