{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE GADTs #-}
module GadtSpec where

-- Reading https://en.m.wikibooks.org/wiki/Haskell/GADT

import Test.Hspec

-- Interstingly, (Eq a) must be added here, not in eval
-- data Exp a where
data Exp a where
  I :: Int -> Exp Int
  B :: Bool -> Exp Bool
  Add :: Exp Int -> Exp Int -> Exp Int
  Mul :: Exp Int -> Exp Int -> Exp Int
  Eq :: (Eq a) =>Exp a -> Exp a -> Exp Bool


add :: Exp Int -> Exp Int -> Exp Int
add = Add

mul :: Exp Int -> Exp Int -> Exp Int
mul = Mul

i :: Int -> Exp Int
i = I

b :: Bool -> Exp Bool
b = B


eval :: Exp a -> a
eval (I iv) = iv
eval (B bv) = bv
eval (Add i1 i2) = eval i1 + eval i2
eval (Mul i1 i2) = eval i1 * eval i2

eval (Eq  e1 e2) = (eval e1) == (eval e2)

spec :: Spec
spec = do
  it "evaluates expressions" $ do
    (eval ((I 10) `Add` (I 25))) `shouldBe` 35

  it "evaluates nested expression" $ do
    let e35 =  (I 10) `Add` (I 25)
        e35' = (I 15) `Add` (I 20)
    eval (Eq e35 e35') `shouldBe` True


  it "compares booleans" $ do
    eval (B True) `shouldBe` True

  -- does not even compile!
  -- it "cannot mix Int and Booleans" $ do
  --   eval ((I 10) `Add` (B True)) `shouldBe` Nothing
