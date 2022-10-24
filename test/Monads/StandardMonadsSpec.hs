{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Monads.StandardMonadsSpec where

import Test.Hspec
import Control.Monad (guard)
import GHC.Base (MonadPlus)
import Control.Monad (MonadPlus(mzero))

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

-- [1,2,3] [4,5,6]
--
conc :: List' a -> List' a -> List' a
conc a Empty = a
conc Empty b = b
conc (List' a restA) b =
  a +: conc restA b

bind :: List' a -> (a-> List' v) -> List' v
bind Empty _ = Empty
bind (List' v restV) f =
  let rs = f v in
    conc rs (bind restV f)


instance Monad List' where
  return = pure
  v >>= f = bind v f

concat' :: List' (List' a) -> List' a
concat' Empty = Empty
concat' (List' v rest) = conc v (concat' rest)

n7 :: [Int]
n7 = do
  x <- [1..] :: [Int]
  if '7' `elem` show x
    then return x
    else []

guard' :: MonadPlus m => Bool -> m ()
guard' True  = return ()
guard' False = mzero

spec :: Spec
spec = do
  it "list as an applicative" $ do
    List' (*2) Empty <*> List' 10 Empty `shouldBe` List' 20 Empty
    let f = (*2) +: (*4) +: Empty
        v = (20 +: 30 +: Empty) :: List' Int in
      f <*> v  `shouldBe` 40 +: 60 +: 80 +: 120 +: Empty

  it "list as a monad" $ do
    let v = 10 +: 20 +: Empty
        f x = (x*2) +: (x*3) +: Empty in
      (v >>= f) `shouldBe` (20 +: 30 +: 40 +: 60 +: Empty)

  it "concat of a list" $ do
    let v1 = 1 +: 2  +: Empty  :: List' Int
        v2 = 3 +: 4  +: Empty  :: List' Int
        f = v1 +: v2 +: Empty  :: List' (List' Int)  in
      concat' f `shouldBe` (1 +: 2 +: 3 +: 4 +: Empty)


  it "chaining multiple lists" $ do
    ([1,2]      >>= (\n ->
     ['a', 'b'] >>= \ch ->
     return (n, ch)))

      `shouldBe` (
      do
        n <- [1,2]
        ch <- ['a', 'b']
        return (n, ch))

  it "filters out numbers not containing a 7" $ do
    take 10 ( [x | x <- [1..], '7' `elem` show x ]) `shouldBe` [7,17,27,37,47,57,67,70,71,72]

  it "filters out numbers not containing a 7, with do notation" $ do
    take 10 n7 `shouldBe` [7,17,27,37,47,57,67,70,71,72]

  it "filters out numbers not containing a 7, with monads" $ do
    take 10 ([1..] >>= (\i -> if '7' `elem` show i then return i else [])) `shouldBe` [7,17,27,37,47,57,67,70,71,72]

  it "filters out numbers not containing a 7 using MonadPlus" $ do
    take 10 (
      [1..] >>= (\i -> guard ('7' `elem` show i) >> return i)) `shouldBe` [7,17,27,37,47,57,67,70,71,72]

  it "implements guard" $ do
    (guard' (2 > 0) >> Just "hey") `shouldBe` Just "hey"
    (guard' (2 < 0) >> Just "hey") `shouldBe` Nothing
