{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fmap" #-}

module Monads.MonadicFunctionsSpec where

import Test.Hspec
import Control.Monad.Writer




spec :: Spec
spec = do
  it "runs liftM" $ do
    (liftM (*2) (return 3)) `shouldBe` (fmap (*2) (Just 3))

  it "runs liftM with Writer" $ do
    runWriter (liftM not $ writer (True, "negated")) `shouldBe` (False, "negated")
