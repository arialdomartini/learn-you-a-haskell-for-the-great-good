{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Monads.StackStateSpec where

import Test.Hspec ( shouldBe, it, Spec )
import Control.Monad.State

type Stack a = [a]

push' :: a -> State (Stack a) ()
push' a = state (\xs -> ((), a:xs ))

pop' :: State (Stack a) a
pop' = state (\(x:xs) -> (x, xs))

useStack :: State (Stack Int) Int
useStack = do
    push' 1
    push' 2
    push' 3
    a <- pop'
    return a

spec :: Spec
spec = do
  it "use a stack with Reader" $
   runState useStack [] `shouldBe` (3, [2,1])
