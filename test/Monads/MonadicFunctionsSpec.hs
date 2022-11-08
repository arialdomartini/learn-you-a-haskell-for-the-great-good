{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fmap" #-}

module Monads.MonadicFunctionsSpec where

import Test.Hspec
import Control.Monad.Writer


spec :: Spec
spec = do
  it "liftM is fmap for Monads" $ do
    liftM (*2) (return 3) `shouldBe` fmap (*2) (Just 3)

  it "runs liftM with Writer" $ do
    runWriter (liftM not $ writer (True, "negated")) `shouldBe` (False, "negated")

  it "ap is <*> for Monads" $ do
    let f a b c = a + b + c in
      Just f `ap` Just 1 `ap` Just 2 `ap` Just 3 `shouldBe` Just f <*> Just 1 <*> Just 2 <*> Just 3
