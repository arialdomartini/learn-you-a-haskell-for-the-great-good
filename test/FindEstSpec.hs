module FindEstSpec where

import Test.Hspec
import Chapter7.FindEst

main :: IO ()


main = hspec spec

spec :: Spec
spec = do

  it "" $ do
    gimmefive `shouldBe` 5
