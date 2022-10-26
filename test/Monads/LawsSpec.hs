{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Monad law, right identity" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use >=>" #-}
module Monads.LawsSpec where

import Test.Hspec
import Control.Monad ((>=>), (<=<))

-- >>= :: m a -> (a -> m b) -> m b

f' :: Int -> Maybe Int
f' n = Just $ n * 42

{-

Monads Laws

return x >>= f  = f x
m x >>= return  = m x

-}

double :: Int -> Maybe Int
double x = Just (x * 2)
prec   :: Int -> Maybe Int
prec   x = Just (x - 1)

f :: Float -> Maybe String
f x = Just $ show x

g :: String -> Maybe Int
g s = Just $ length s

h :: Int -> Maybe Int
h n = return $ n * 2


(>=>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
ff >=>> gg = \x -> ff x >>= gg
infixr 1 <=<<

(<=<<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
gg <=<< ff = \x -> ff x >>= gg

infixr 1 >=>>

spec :: Spec
spec = do
  -- applying return on the left of  a bind, is like just applying the function
  it "left identity" $ do
    (return 3 >>= f')     `shouldBe`    f' 3

  -- applying return on the right of a bind does noth change the monadic input
  it "right identity" $ do
    (Just 3 >>= return)  `shouldBe`    Just 3

  it "associativity" $ do
      ((Just 3 >>= (\x ->   double x)) >>= prec) `shouldBe`
       (Just 3 >>= (\x ->   double x  >>= prec))

  it "Kleisly operators" $ do
    (f >=> g) 3.14 `shouldBe` Just 4
    (g <=< f) 3.14 `shouldBe` Just 4

  it "Kleisly operators implemented by hand" $ do
    (f >=>> g) 3.14 `shouldBe` (f >=> g) 3.14
    (g <=<< f) 3.14 `shouldBe` (g <=< f) 3.14

  it "Monad laws expressed with the Kleisly operator" $ do
    (return >=> f)         3 `shouldBe` f 3
    (f >=> return)         3 `shouldBe` f 3
    ((f >=> g) >=>    h)  10 `shouldBe`
     (f >=>    (g >=> h)) 10
