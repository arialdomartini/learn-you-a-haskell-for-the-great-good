{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module GadtSpec where

-- Reading https://en.m.wikibooks.org/wiki/Haskell/GADT

import Test.Hspec

data Exp =
   I Int
 | B Bool
 | Exp `Add` Exp
 | Exp `Mul` Exp
 | Exp `Eq` Exp
 deriving (Eq)

data BoolInt = Bool Bool | Int Int deriving (Show, Eq)

eval :: Exp -> Maybe BoolInt
eval (I i) = Just $ Int i
eval (B b) = Just $ Bool b
eval (e1 `Eq` e2) =
  let re1 = (eval e1)
      re2 = (eval e2) in
      case (re1, re2) of
        (Just (Bool b1),  Just (Bool b2))  -> Just (Bool  (b1 == b2))
        (Just (Int i1), Just (Int i2)) -> Just (Bool (i1 == i2))
        (_,_) -> Nothing

eval (e1 `Add` e2) =
  ops e1 e2 (+)

eval (e1 `Mul` e2) =
  ops e1 e2 (*)

-- (Maybe (Either Bool Int) -> Maybe (Either Bool Int) -> Maybe a)
ops :: Exp -> Exp -> (Int -> Int -> Int) -> Maybe BoolInt
ops e1 e2 op =
  let re1 = (eval e1)
      re2 = (eval e2) in
      case (re1, re2) of
        (Just (Int i1), Just (Int i2) ) -> Just (Int (i1 `op` i2))
        --(Just es1, Just es2) -> ops re1 re2 op
        (_,_) -> Nothing


spec :: Spec
spec = do
  it "evaluates expressions" $ do
    (eval ((I 10) `Add` (I 25))) `shouldBe` Just (Int 35)

  it "compares booleans" $ do
    eval (B True) `shouldBe` Just (Bool True)

  it "cannot mix Int and Booleans" $ do
    eval ((I 10) `Add` (B True)) `shouldBe` Nothing
