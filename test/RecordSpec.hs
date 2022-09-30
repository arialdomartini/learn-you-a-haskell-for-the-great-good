module RecordSpec where

import Test.Hspec

data Person =
  Person
  {
    firstName :: String
  , secondName :: String
  , height :: Int }

spec :: Spec
spec = do
  it "can use records" $ do
    let john = Person { firstName = "John", secondName = "Doh", height = 182 } in
      firstName john `shouldBe` "John"
