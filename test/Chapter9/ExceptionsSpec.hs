module Chapter9.ExceptionsSpec where

import Chapter9.Exceptions
import Test.Hspec

spec = do
  it "divides by 2" $ do
    10 `div'` 2 `shouldBe` Just 5
  it "does not divide by 0" $ do
    10 `div'` 0 `shouldBe` Nothing
