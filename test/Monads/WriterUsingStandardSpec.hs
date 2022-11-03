module Monads.WriterUsingStandardSpec where

import Test.Hspec
import Control.Monad.Writer

f :: Writer [String] Int
f =
  return "Hey" >>=
  \s -> writer (s ++ "!", ["screaming"]) >>=
  \s -> writer (length s, ["calculating length"])

f' :: Writer [String] Int
f' = do
  let s = "Hey"
  s' <- writer (s ++ "!", ["screaming"])
  writer (length s', ["calculating length"])

spec :: Spec
spec = do
  it "bind monadic functions" $ do
    let (result, logs) = runWriter f
      in do logs `shouldBe` ["screaming", "calculating length"]
            result `shouldBe` 4

  it "bind monadic functions, with do notation" $ do
    let (result, logs) = runWriter f'
      in do logs `shouldBe` ["screaming", "calculating length"]
            result `shouldBe` 4
