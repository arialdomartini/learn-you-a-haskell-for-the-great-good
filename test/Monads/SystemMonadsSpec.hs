{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Monads.SystemMonadsSpec where

import Test.Hspec

data Maybe' a = Nothing' | Just' a deriving (Show, Eq)

class Applicative' f where
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative' m => Monad' m where
  (>>>=) :: m a -> (a -> m b) -> m b

instance Applicative' Maybe' where
  _ <*> Nothing' = Nothing'
  Nothing' <*> _ = Nothing'
  Just' f <*> Just' a = Just' (f a)

instance Monad' Maybe' where
  Nothing' >>>= _ = Nothing'
  Just' a >>>= f = f a

maybeDouble :: Int -> Maybe' Int
maybeDouble n = if n < 100 then Just' (n * 2) else Nothing'


doubleIfSmall :: Int -> Maybe' Int
doubleIfSmall n = if n < 100 then Just' (n * 2) else Nothing'

minus1IfEven :: Int -> Maybe' Int
minus1IfEven n =   if even n then Just' (n -1) else Nothing'

toStringIfOdd :: Int -> Maybe' String
toStringIfOdd n = if odd n then Just' (show n) else Nothing'

spec :: Spec
spec = do
  it "Maybe is a Monad" $ do
    Just' 10 >>>= maybeDouble `shouldBe` Just' 20
    Nothing' >>>= maybeDouble `shouldBe` Nothing'

  it "chains monads with bind" $ do
    (Just' 50) >>>= doubleIfSmall >>>= minus1IfEven >>>= toStringIfOdd `shouldBe` Just' "99"
    (Just' 200) >>>= doubleIfSmall >>>= minus1IfEven >>>= toStringIfOdd `shouldBe` Nothing'
