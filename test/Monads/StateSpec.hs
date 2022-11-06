{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Monads.StateSpec where

import Test.Hspec

data State' s a where
  State' :: {runState :: s -> (a, s)} -> State' s a

instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f st =
    State' (\s ->
              let (v,s') = runState st s in
                (f v, s'))

instance Applicative (State' s) where
  pure :: a -> State' s a
  pure a = State' (a, )
  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  sf <*> sv =
    State' (\s ->
              let (f, s') = runState sf s
                  (v, s'') = runState sv s' in
                (f v, s''))

toString :: Float -> State' Int String
toString n = State' (\count -> (show n, count+1))

fs :: State' Int (String -> String)
fs = State' (\count -> ((++ "!"), count + 1))

vs :: State' Int String
vs = State' (\count -> ("Hey", count+1))

spec :: Spec
spec = do
  it "runState" $ do
    runState (toString 5.3) 10 `shouldBe` ("5.3", 11)

  it "has an instance of Functor" $ do
    let r = fmap (++"!") (toString 5.3) in
        runState r 99 `shouldBe` ("5.3!", 100)

  it "has an instance of Applicative" $ do
    runState (fs <*> vs) 100 `shouldBe` ("Hey!", 102)
