module RecordSpec where

import Test.Hspec

data Person =
  Person
  {
    firstName :: String
  , secondName :: String
  , height :: Int }


-- This record could not be defined, because it would create
-- clashing functions
-- A possible alternative approach is described here:
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/duplicate_record_fields.html#duplicate-record-fields
-- data RecordWithConflictingFieldName =
--   AnotherRecord
--   {
--     firstName :: String
--   , secondName :: String
--   , height :: Int }

spec :: Spec
spec = do
  it "can use records" $ do
    -- fields are not positional
    let john = Person { secondName = "Doh", firstName = "John", height = 182 } in
      firstName john `shouldBe` "John"
