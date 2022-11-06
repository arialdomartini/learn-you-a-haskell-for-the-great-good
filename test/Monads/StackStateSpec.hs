{-# OPTIONS_GHC -Wno-type-defaults #-}
module Monads.StackStateSpec where

import Test.Hspec ( shouldBe, it, Spec )
import Control.Monad.State

newtype Stack a = Stack [a] deriving (Eq, Show)

push' :: a -> State (Stack a) ()
push' a = state (\s ->
                let Stack xs = s in
                   ((), Stack (a:xs) ))

pop' :: State (Stack a) a
pop' = state (\s ->
                 let Stack (x:xs) = s in
                   (x, Stack xs))

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
   runState useStack (Stack []) `shouldBe` (3, Stack [2,1])
