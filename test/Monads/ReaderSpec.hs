module Monads.ReaderSpec where

import Test.Hspec

f :: String -> (String -> Int)
f s = \x -> length (s ++ x)

g :: Int -> (String -> Int)
g n = \x -> (length x) + (n *2)

spec :: Spec
spec = do
  it "binds 2 functions" $ do
    ((f "hey") >>= g)("joe") `shouldBe` 15
