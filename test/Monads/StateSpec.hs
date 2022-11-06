{-# LANGUAGE GADTs #-}
module Monads.StateSpec where

import Test.Hspec

data State' a s where
  State' :: {runState :: s -> (a, s)} -> State' a s

len :: Float -> State' String Int
len n = State' (\count -> (show n, count+1))

spec :: Spec
spec = do
  it "runState" $ do
    runState (len 5.3) 10 `shouldBe` ("5.3", 11)
