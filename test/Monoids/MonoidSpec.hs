{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Monoid law, left identity" #-}

module Monoids.MonoidSpec where

import Test.Hspec
import Control.Exception
import Control.Applicative (Applicative (liftA2))

type A = String
type B = String
f :: A -> B
f s = s ++ "!"

newtype A' = A' {getValueA :: String } deriving (Show, Eq)
newtype B' = B' {getValueB :: String } deriving (Show, Eq)

f' :: A' -> B'
-- f' s = s ++ "!"  <- this does not work
-- f' s = B' $ getValueA s ++ "!"
f' (A' s) = B' $ s ++ "!"

newtype Pair b a = Pair (a, b) deriving (Show, Eq)
instance Functor (Pair b)  where
--fmap :: (a -> b) -> f a -> f b
  fmap f (Pair (a,b)) = Pair (f a, b)

data CoolBool = CoolBool { getCoolBool:: Bool}
newtype LazyCoolBool = LazyCoolBool { getLazyCoolBool:: Bool}

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m

newtype SumInt = SumInt Int deriving (Show, Eq)

instance Monoid' SumInt where
  mempty' = SumInt 0
  mappend' (SumInt a) (SumInt b) = SumInt (a + b)
  mconcat' = foldr mappend' mempty'


newtype Prod a = Prod a deriving (Show, Eq)
instance Num a => Semigroup (Prod a) where
  (Prod a) <> (Prod b) = Prod (a * b)

instance Num a => Monoid (Prod a) where
  mempty = Prod 1
  -- mappend = (Data.Semigroup.<>) -- this is the canonical monoid definition, which can be omitted

newtype Any' = Any' Bool deriving (Show, Eq)
instance Semigroup Any' where
  Any' a1 <> Any' a2 = Any' (a1 || a2)

instance Monoid Any' where
  mempty = Any' True
  mappend = (<>)

data Maybe' a = Just' a | Nothing' deriving (Show, Eq)
instance Monoid a => Semigroup (Maybe' a) where
  (Just' a) <> (Just' b) = Just' (a <> b)
  _ <> _ = Nothing'

instance Monoid a => Monoid (Maybe' a) where
  mempty = mempty
  mappend = (<>)

newtype First a = First (Maybe a) deriving (Show, Eq)
instance Semigroup (First a) where
  (First (Just a)) <> _ = First (Just a)
  First Nothing <> s = s

instance Monoid (First a) where
  mempty = First Nothing
  mappend = (<>)


newtype Last a = Last { getLast :: Maybe a } deriving (Show, Eq)
instance Semigroup (Last a) where
  _ <> Last (Just a) = Last (Just a)
  f <> Last Nothing = f
instance Monoid (Last a) where
  mempty = Last Nothing
  mappend = (<>)

spec :: Spec
spec = do
  it "type alias are loosely checked" $ do
     let s = "Hey" :: B
         b = "Hey!" :: A
      in f s `shouldBe` b  -- It works. Too bad.

  it "newtypes are more strict" $ do
     let s = A' "Hey"
         b = B' "Hey!"
      in f' s `shouldBe` b

  it "implements Functor for tuples using a newtype for inverting the type parameters" $ do
    let pair = Pair ("Joe", "Hey")
        f s = length s in
      fmap f pair `shouldBe` Pair (3, "Hey")

  it "demostrates newtype is lazy" $ do
    let fStrict (CoolBool    _ ) = True
        fLazy   (LazyCoolBool _) = True in do
      -- both the following work
      fStrict (CoolBool undefined)   `shouldBe` True
      fLazy (LazyCoolBool undefined) `shouldBe` True

      evaluate (fStrict undefined) `shouldThrow` anyErrorCall
      fLazy undefined `shouldBe` True


  it "defines what a monoid is" $ do
    mappend' mempty' (SumInt 22) `shouldBe` SumInt 22
    mappend' (SumInt 10) (SumInt 22) `shouldBe` SumInt 32

  it "mconcat is a repeated application of mappend" $ do
    mconcat' [SumInt 1, SumInt 2, SumInt 10] `shouldBe` SumInt 13

  it "verifies the Monoid Laws" $ do
    -- left and right unit law
    mappend mempty   ["hey"] `shouldBe` ["hey"]
    mappend ["hey"]  mempty  `shouldBe` ["hey"]

    -- associative law
    (["hey"] `mappend`  ["joe"])  `mappend` ["how", "are", "you"] `shouldBe`
     ["hey"] `mappend`  (["joe"]  `mappend` ["how", "are", "you"])


  it "List is an instance of Monoid" $ do
    let empty = []
        binary = (++)
    mappend mempty [3,4] `shouldBe` binary empty [3,4]


  it "Product type as a Monoid" $ do
    mappend (Prod 2) (Prod 42) `shouldBe` mappend mempty (Prod (2 * 42))
    Prod 2 <> Prod 42 `shouldBe` mempty <> Prod (2 * 42)
    (mconcat . fmap Prod) [1,2,3,4] `shouldBe` Prod (1*2*3*4)
    (foldr1 (<>) . fmap Prod) [1,2,3,4] `shouldBe` Prod (1*2*3*4)

  it "Bool (with Any) is an instance of Monoid" $ do
    (foldr1 (<>) . fmap Any') [True, True, True]   `shouldBe` Any' True

  it "Maybe as a Monoid" $ do
    Just' (Prod 12) <> Just' (Prod 2) `shouldBe` Just' (Prod 24)
    Just' (Prod 12) <> Nothing' `shouldBe` Nothing'

  it "Maybe as a Monoid, with applicative" $ do
    liftA2 (<>) (pure (Prod 12)) (pure (Prod 2)) `shouldBe` Just (Prod 24)

  it "First on Maybe as a Monoid" $ do
    First (Just 4) `mappend` First (Just 5) `shouldBe` First (Just 4)
    First Nothing `mappend` First (Just 5) `shouldBe` First (Just 5)
    mempty `mappend` First (Just 5) `shouldBe` First (Just 5)

  it "Last on Maybe as a Monoid" $ do
    (getLast . mconcat . fmap Last) [Just 1, Just 2, Just 3] `shouldBe` Just 3
