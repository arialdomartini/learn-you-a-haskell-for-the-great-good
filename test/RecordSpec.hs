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
    -- fields are not positional
    let john = Person { secondName = "Doh", firstName = "John", height = 182 } in
      firstName john `shouldBe` "John"
