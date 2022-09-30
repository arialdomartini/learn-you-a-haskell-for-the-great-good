{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module GadtSpec where

-- Reading https://en.m.wikibooks.org/wiki/Haskell/GADT

import Test.Hspec

data Exp =
   I Int
 | Add Exp Exp
 | Mul Exp Exp
 deriving (Eq)

eval :: Exp -> Int
eval (I i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

spec :: Spec
spec = do
  it "evaluates expressions" $ do
    eval ((I 10) `Add` (I 25)) `shouldBe` 34
