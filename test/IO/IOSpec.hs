module IO.IOSpec where

import Test.Hspec

-- A function returning IO
f :: IO String
f = do return "mario"


spec :: Spec
spec = do
  -- asserting an IO
  it "cracks open an IO" $ do
    result <- f
    result `shouldBe` "mario"
