module TypeSynonymsSpec where

import Test.Hspec
import qualified Data.Map as Map
import Data.Function


type PhoneNumber = String
type Name = String
type PhoneBook = Map.Map Name PhoneNumber
type Message = String

phoneBook :: PhoneBook
phoneBook = [ ("Pula", "113"), ("Mario", "0573 28 0 21") ] & Map.fromList

call :: PhoneBook -> Name -> Message
call p name =
  case Map.lookup name p of
    (Just number) -> "I've called " ++ number
    _ -> "I don't know the number!"

spec :: Spec
spec = do
  it "uses type synonyms" $ do
    let pula = "Pula" :: Name
        pope = "The Pope" :: String in do

        phoneBook `call` pula `shouldBe` "I've called 113"
        phoneBook `call` pope `shouldBe` "I don't know the number!"

  -- Values of different synonyms of the same type are entirely compatible
  -- See https://wiki.haskell.org/Type_synonym
  it "type aliases can be mixed" $ do
    let pula = "Pula" :: PhoneNumber in
        phoneBook `call` pula `shouldBe` "I've called 113"
