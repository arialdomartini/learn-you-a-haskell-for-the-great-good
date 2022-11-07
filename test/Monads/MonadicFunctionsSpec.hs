{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fmap" #-}
module Monads.MonadicFunctionsSpec where

import Test.Hspec
import Control.Monad (liftM)


spec :: Spec
spec = do
  it "liftM" $ do
    liftM (*2) (return 3) `shouldBe` fmap (*2) (Just 3)
