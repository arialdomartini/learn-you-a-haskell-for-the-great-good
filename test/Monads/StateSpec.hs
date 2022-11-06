{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module Monads.StateSpec where

import Test.Hspec

data State' s a where
  State' :: {runState :: s -> (a, s)} -> State' s a

instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f st =
    State' (\s ->
              let fs = runState st
                  (v,s') = fs s in
                (f v, s'))

toString :: Float -> State' Int String
toString n = State' (\count -> (show n, count+1))

spec :: Spec
spec = do
  it "runState" $ do
    runState (toString 5.3) 10 `shouldBe` ("5.3", 11)

  it "has an instance of Functor" $ do
    let r = fmap (++"!") (toString 5.3) in
        runState r 99 `shouldBe` ("5.3!", 100)
