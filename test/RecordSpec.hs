{-# LANGUAGE DisambiguateRecordFields #-}

module RecordSpec where

import Test.Hspec
import Record2

data Person = Person { firstName :: String }

spec :: Spec
spec = do
  it "can use records" $ do
    -- fields are not positional
    let
      john = Person { firstName = "John" }
      other = AnotherRecord { firstName = "foo" } in
      do
        RecordSpec.firstName john `shouldBe` "John"
        Record2.firstName other `shouldBe` "foo"
