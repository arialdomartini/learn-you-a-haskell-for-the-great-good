{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fmap" #-}

module Monads.MonadicFunctionsSpec where

import Test.Hspec
import Control.Monad.Writer
import Control.Applicative (liftA2)

f :: Int -> Int -> Int
f a b = a + b

fA2 :: Maybe Int -> Maybe Int -> Maybe Int
fA2 = liftA2 f

fM2 :: Maybe Int -> Maybe Int -> Maybe Int
fM2 = liftM2 f


spec :: Spec
spec = do
  it "liftM is fmap for Monads" $ do
    liftM (*2) (return 3) `shouldBe` fmap (*2) (Just 3)

  it "runs liftM with Writer" $ do
    runWriter (liftM not $ writer (True, "negated")) `shouldBe` (False, "negated")

  it "ap is <*> for Monads" $ do
    Just f `ap` Just 1 `ap` Just 2 `shouldBe` Just f <*> Just 1 <*> Just 2

  it "liftA2 for Monads" $ do
    f 1 2 `shouldBe` 3
    fA2 (Just 1) (Just 2) `shouldBe` Just 3
    fM2 (Just 1) (Just 2) `shouldBe` Just 3
