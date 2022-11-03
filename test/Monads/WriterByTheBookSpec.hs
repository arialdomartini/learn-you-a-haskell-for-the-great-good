{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module Monads.WriterByTheBookSpec where

import Test.Hspec
import GHC.Float (int2Float)

data Writer' w a where
  Writer' :: {runWriter :: (a, w)} -> Writer' w a
  deriving (Eq, Show)

instance Functor (Writer' w) where
  fmap f (Writer' (a, w)) = Writer' (f a, w)

instance Monoid w => Applicative (Writer' w) where
  pure :: a -> Writer' w a
  pure a = Writer' (a, mempty)
  (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
  (Writer' (f, w)) <*> (Writer' (a, w')) = Writer' (f a, w `mappend` w')

instance Monoid w => Monad (Writer' w) where
  (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
  (Writer' (a, w)) >>= f =
    let Writer' (r, w') = f a in
      Writer' (r, w `mappend` w')

type Logs = [String]

len :: String -> Writer' Logs Int
len s = Writer' (length s, ["Calculated the length"])

halve :: Int -> Writer' Logs Float
halve n = Writer' (int2Float n / 2, ["Divided by 2"])


withDoNotation :: String -> Writer' Logs Float
withDoNotation s = do
  l <- len s
  halve l


spec :: Spec
spec = do
  it "should bind with Writer" $ do
    runWriter (len "Hey Joe!" >>= halve) `shouldBe` (4, ["Calculated the length", "Divided by 2"])

  it "applies Writer using the do notation" $ do
    withDoNotation "Hey Joe!" `shouldBe` Writer' (4, ["Calculated the length", "Divided by 2"])
