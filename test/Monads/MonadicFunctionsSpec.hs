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


ff :: Int -> Maybe Int
ff n = return (n * 2)

bind' :: Monad m => m a -> (a -> m b) -> m b
bind' m f' = (join . fmap f') m

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' f' (x:xs) = do
  r <- f' x
  rest <- filterM' f' xs
  if r
    then return (x : rest)
    else return rest

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

foldM' :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM' g b foldable =
 foldl g' (return b) foldable where
  g' b' a = do
    v <- b'
    g v a

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


  it "bind is join . fmap" $ do
    (Just 3 >>= ff) `shouldBe` Just 6

  it "filterM is filter for Monads" $ do
    filter odd [1,2,3,4] `shouldBe` [1,3]
    runWriter (filterM oddM [1,2,3,4]) `shouldBe` ([1,3], ["1 is odd", "2 is even", "3 is odd", "4 is even"])

  it "implements filterM" $ do
    runWriter (filterM oddM [1,2,3,4]) `shouldBe` runWriter (filterM' oddM [1,2,3,4])

  it "calculates the powerset of a set" $ do
    powerset [1,2,3] `shouldBe` [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

  it "implements foldM" $ do
    -- foldM (b -> a -> m b) b (t a)
    foldM  (\i acc -> Just (i + acc)) 0 [1,2,3,4] `shouldBe` Just 10
    foldM' (\i acc -> Just (i + acc)) 0 [1,2,3,4] `shouldBe` Just 10

  it "uses of foldM, sum numbers, but fail if any n > 9 " $ do
    foldM (\i acc -> if i > 9 then Nothing else Just (i + acc)) 0 [1,2,3,4] `shouldBe` Just 10
    foldM (\i acc -> if i > 9 then Nothing else Just (i + acc)) 0 [1,9,3,4] `shouldBe` Nothing

  it "folds keepign logs" $ do
    runWriter (foldM (\acc i -> writer (i+acc, ["+" ++ show i])) 0 [1,2,3])
      `shouldBe` (6, ["+1", "+2", "+3"])
