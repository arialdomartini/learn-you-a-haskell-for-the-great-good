{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monad law, left identity" #-}
module Monads.LawsSpec where

import Test.Hspec

-- >>= :: m a -> (a -> m b) -> m b

f :: Int -> Maybe Int
f n = Just $ n * 42

spec :: Spec
spec = do
  it "left identity" $ do
    (return 3 >>= f)
      `shouldBe`
     f 3
