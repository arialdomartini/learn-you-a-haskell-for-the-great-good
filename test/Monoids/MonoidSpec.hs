{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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

newtype Pair b a = Pair (a, b) deriving (Show, Eq)
instance Functor (Pair b)  where
--fmap :: (a -> b) -> f a -> f b
  fmap f (Pair (a,b)) = Pair (f a, b)

spec :: Spec
spec = do
  it "type alias are loosely checked" $ do
     let s = "Hey" :: B
         b = "Hey!" :: A
      in f s `shouldBe` b  -- It works. Too bad.

  it "newtypes are more strict" $ do
     let s = A' "Hey"
         b = B' "Hey!"
      in f' s `shouldBe` b

  it "implements Functor for tuples using a newtype for inverting the type parameters" $ do
    let pair = Pair ("Joe", "Hey")
        f s = length s in
      fmap f pair `shouldBe` Pair (3, "Hey")
