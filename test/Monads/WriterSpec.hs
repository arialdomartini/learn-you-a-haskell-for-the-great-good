{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Monads.WriterSpec where

import Test.Hspec

data Writer' l o = Writer' (l,o) deriving (Show, Eq)


instance Functor (Writer' l) where
  fmap :: (a -> b) -> Writer' l a -> Writer' l b
  fmap f (Writer' (l,o)) =
    Writer' (l, f o)

instance Monoid l => Applicative (Writer' l) where
  pure :: a -> Writer' l a
  pure a = Writer' (mempty, a)
  (<*>) :: Writer' l (a -> b) -> Writer' l a -> Writer' l b
  (Writer' (l1, f1)) <*> (Writer' (l2, v2)) =
    Writer' (l1 `mappend` l2, f1 v2)

instance (Monoid l, Applicative (Writer' l)) => Monad (Writer' l) where
  (>>=) :: Writer' l a -> (a -> Writer' l b) -> Writer' l b
  (Writer' (l1, o1)) >>= f =
    let (Writer' (l2, o2)) = f o1 in
      Writer' (l1 `mappend` l2, o2)

f :: Int -> Writer' String Int
f n = Writer' ("-doubled", n*2 )



spec :: Spec
spec = do
  it "is a functor" $ do
    fmap (*2) (Writer' ("as-is", 4)) `shouldBe` Writer' ("as-is", 8)

  it "is a monad" $ do
    (Writer' ("4", 4) >>= f) `shouldBe` Writer' ("4-doubled", 8)
