module Monoids.MonoidSpec where

import Test.Hspec

type A = String
type B = String
f :: A -> B
f s = s ++ "!"

newtype A' = A' {getValueA :: String } deriving (Show, Eq)
newtype B' = B' {getValueB :: String } deriving (Show, Eq)

f' :: A' -> B'
-- f' s = s ++ "!"  <- this does not work
-- f' s = B' $ getValueA s ++ "!"
f' (A' s) = B' $ s ++ "!"

spec :: Spec
spec = do
  it "type alias are loosely checked" $ do
     let s = A' "Hey"
         b = B' "Hey!"
      in f' s `shouldBe` b
