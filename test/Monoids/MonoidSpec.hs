{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Monoids.MonoidSpec where

import Test.Hspec
import Control.Exception

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

data CoolBool = CoolBool { getCoolBool:: Bool}
newtype LazyCoolBool = LazyCoolBool { getLazyCoolBool:: Bool}

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m

newtype SumInt = SumInt Int deriving (Show, Eq)

instance Monoid' SumInt where
  mempty' = SumInt 0
  mappend' (SumInt a) (SumInt b) = SumInt (a + b)
  mconcat' = foldr mappend' mempty'


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

  it "demostrates newtype is lazy" $ do
    let fStrict (CoolBool    _ ) = True
        fLazy   (LazyCoolBool _) = True in do
      -- both the following work
      fStrict (CoolBool undefined)   `shouldBe` True
      fLazy (LazyCoolBool undefined) `shouldBe` True

      evaluate (fStrict undefined) `shouldThrow` anyErrorCall
      fLazy undefined `shouldBe` True


  it "defines what a monoid is" $ do
    mappend' mempty' (SumInt 22) `shouldBe` SumInt 22
    mappend' (SumInt 10) (SumInt 22) `shouldBe` SumInt 32

  it "mconcat is a repeated application of mappend" $ do
    mconcat' [SumInt 1, SumInt 2, SumInt 10] `shouldBe` SumInt 13
