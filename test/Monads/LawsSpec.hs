{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Monad law, right identity" #-}
module Monads.LawsSpec where

import Test.Hspec

-- >>= :: m a -> (a -> m b) -> m b

f :: Int -> Maybe Int
f n = Just $ n * 42

{-

Monads Laws

return x >>= f  = f x
m x >>= return  = m x

-}

spec :: Spec
spec = do
  -- applying return on the left of  a bind, is like just applying the function
  it "left identity" $ do
    (return 3 >>= f)     `shouldBe`    f 3

  -- applying return on the right of a bind does noth change the monadic input
  it "right identity" $ do
    (Just 3 >>= return)  `shouldBe`    Just 3
