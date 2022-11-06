module Monads.ReaderSpec where

import Test.Hspec

f :: String -> (String -> Int)
f s = \x -> length (s ++ x)

g :: Int -> (String -> Int)
g n = \x -> (length x) + (n *2)

bound :: String -> Int
bound = do
  r  <- f "hey"
  r' <- g r
  return r'

spec :: Spec
spec = do
  it "binds 2 functions" $ do
    ((f "hey") >>= g)("joe") `shouldBe` 15

  it "binds 2 functions using the do notation" $ do
    bound "joe" `shouldBe` 15
