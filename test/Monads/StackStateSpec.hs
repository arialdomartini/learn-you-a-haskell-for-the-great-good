{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monads.StackStateSpec where

import Test.Hspec ( shouldBe, it, Spec )
import Control.Monad.State
import qualified Control.Monad.Identity as Data.Functor

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

-- This is required by (x:xs) <- get in pop'': I don't know why!
instance MonadFail Data.Functor.Identity where
  fail = error "should not happen"


anotherUseOfStack :: State (Stack Int) Int
anotherUseOfStack = do
  push' 100
  _ <- pop' --100
  pop'      -- 2

combined :: State (Stack Int) Int
combined = state (\i ->
  let (_, s) = runState useStack i in
    runState anotherUseOfStack s)

combinedDo :: State (Stack Int) Int
combinedDo = do
  _ <- useStack
  anotherUseOfStack

pop'' :: State (Stack Int) Int
pop'' = do
  (x:xs) <- get
  put xs
  return x

push'' :: Int -> State (Stack Int) ()
push'' x = do
  xs <- get
  put (x:xs)
  return ()

useMonadState :: State (Stack Int) Int
useMonadState = do
  push'' 1
  push'' 2
  push'' 3
  pop''

spec :: Spec
spec = do
  it "use a stack with Reader" $ do
   runState useStack [] `shouldBe` (3, [2,1])

  it "combines 2 stateful computations" $ do
    runState combined [] `shouldBe` (2, [1])

  it "combines 2 stateful computations, using do notation" $ do
    runState combinedDo [] `shouldBe` (2, [1])

  it "implements pop and put with MonadState" $ do
    runState useMonadState [] `shouldBe` (3, [2,1])
