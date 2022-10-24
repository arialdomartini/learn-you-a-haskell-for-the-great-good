{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Monads.StandardMonadsSpec where

import Test.Hspec

data List' a where
  Empty :: List' a
  List' :: a -> List' a -> List' a
  deriving (Eq, Show)

instance Functor List' where
  fmap _ Empty = Empty
  fmap f (List' a rest) = List' (f a) (fmap f rest)


infixr 9 +:
(+:) :: a -> List' a -> List' a
a +: Empty = List' a Empty
a +: List' b rest = List' a (List' b rest)


mappa :: List' (a->v) -> List' a -> List' a -> List' v
mappa Empty _ _ = Empty
mappa (List' f restF) orig (List' v Empty) =
  f v +: mappa restF orig orig
mappa ff@(List' f _) origV (List' v restV) =
  f v +: mappa ff origV restV


instance Applicative List' where
  pure a = List' a Empty
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  lf <*> lv = mappa lf lv lv

spec :: Spec
spec = do
  it "list as an applicative" $ do
    List' (*2) Empty <*> List' 10 Empty `shouldBe` List' 20 Empty
    let f = (*2) +: (*4) +: Empty
        v = (20 +: 30 +: Empty) :: List' Int in
      f <*> v  `shouldBe` 40 +: 60 +: 80 +: 120 +: Empty
