{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module Monads.WriterByTheBookSpec where

import Test.Hspec
import GHC.Float (int2Float)

newtype Writer' w a where
  Writer' :: {runWriter :: (a, w)} -> Writer' w a
  deriving (Eq, Show)

writer' :: (a, w) -> Writer' w a
writer' (a, w) = Writer' (a, w)

instance Functor (Writer' w) where
  fmap f (Writer' (a, w)) = Writer' (f a, w)

instance Monoid w => Applicative (Writer' w) where
  pure :: a -> Writer' w a
  pure a = writer' (a, mempty)
  (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
  wf <*> wv =
    let (f, w) = runWriter wf
        (a, w') = runWriter wv in
      writer' (f a, w `mappend` w')

instance Monoid w => Monad (Writer' w) where
  (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
  wv  >>= f =
    let (a, w) = runWriter wv
        (r, w') = runWriter (f a) in
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



data CommaSeparated where
  CommaSeparated :: String -> CommaSeparated
  deriving (Eq, Show)

instance Semigroup CommaSeparated where
  CommaSeparated s <> CommaSeparated z = CommaSeparated (s <> ", " <> z)

instance Monoid CommaSeparated where
  mappend = (<>)
  mempty = CommaSeparated (mempty :: String)

useTellNoDo :: Writer' CommaSeparated Int
useTellNoDo =
  writer' (1, CommaSeparated "one") >>=
  tell' (CommaSeparated "intermezzo") >>=
  \j -> writer' (j + 1, CommaSeparated "plus 1")


tell' :: w -> a -> Writer' w a
tell' w a = writer' (a, w)


spec :: Spec
spec = do
  it "should bind with Writer" $ do
    runWriter (len "Hey Joe!" >>= halve) `shouldBe` (4, ["Calculated the length", "Divided by 2"])

  it "applies Writer using the do notation" $ do
    withDoNotation "Hey Joe!" `shouldBe` Writer' (4, ["Calculated the length", "Divided by 2"])

  it "uses tell" $ do
    runWriter useTellNoDo `shouldBe` (2, CommaSeparated "one, intermezzo, plus 1")
