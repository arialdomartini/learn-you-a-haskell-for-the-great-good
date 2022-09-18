module HighOrderFunctionsSpec where

import Test.Hspec

mult3 :: Num a => a -> (a -> (a -> a))
mult3 x y z = x * y * z

spec :: Spec
spec = do
  it "implicitly curries functions" $ do
    mult3 (2::Int) 3 4 `shouldBe` (((mult3 2) 3) 4)
