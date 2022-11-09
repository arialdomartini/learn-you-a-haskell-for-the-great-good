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

join' :: Monad m => m (m a) -> m a
join' a = do
  v <- a
  v

join'' :: Monad m => m (m a) -> m a
join'' mm =
  mm >>= id


oddM :: Int -> Writer [String] Bool
oddM n =
  if odd n
  then writer (True, [(show n) ++ " is odd"])
  else writer (False, [(show n) ++ " is even"])



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

  it "implements join" $ do
    join (Just (Just 1)) `shouldBe` join' (Just (Just 1))
    join (Just Nothing)  `shouldBe` join' (Just Nothing :: Maybe (Maybe Int))
    join Nothing         `shouldBe` join' (Nothing :: Maybe (Maybe Int))
    join [[1,2],[3,4]]   `shouldBe` join' [[1,2],[3,4]]

  it "implements join without do notation" $ do
    join (Just (Just 1)) `shouldBe` join'' (Just (Just 1))
    join (Just Nothing)  `shouldBe` join'' (Just Nothing :: Maybe (Maybe Int))
    join Nothing         `shouldBe` join'' (Nothing :: Maybe (Maybe Int))
    join [[1,2],[3,4]]   `shouldBe` join'' [[1,2],[3,4]]

  it "filterM is filter for Monads" $ do
    filter odd [1,2,3,4] `shouldBe` [1,3]
    runWriter (filterM oddM [1,2,3,4]) `shouldBe` ([1,3], ["1 is odd", "2 is even", "3 is odd", "4 is even"])
