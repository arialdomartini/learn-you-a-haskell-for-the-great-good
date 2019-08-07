module Chapter9.ReverseSpec where

import Test.Hspec
import Chapter9.Reverse

spec = do
  it "reverses a string" $ do
    reverse' "hello world" `shouldBe` "dlrow olleh"
  
