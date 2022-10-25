{-# OPTIONS_GHC -Wno-type-defaults #-}
module Monads.KnightSpec where

import Test.Hspec
import Control.Monad (guard)


data Pos = Pos Int Int deriving (Eq, Show)

pos :: (Int, Int) -> Pos
pos (x,y) = Pos x y

move :: Pos -> [Pos]
move (Pos x y) =
  fmap
    pos
    [ (x + 2, y + 1),
      (x + 2, y - 1),
      (x - 2, y + 1),
      (x - 2, y - 1),
      (x + 1, y + 2),
      (x + 1, y - 2),
      (x - 1, y + 2),
      (x - 1, y - 2)
    ]

between :: Int -> Int -> Int -> Bool
between a b x = x >= a && x <= b

withinBoard :: Pos -> Bool
withinBoard (Pos x y) =
  inBoard x && inBoard y
  where inBoard = between 1 8


walk :: Pos -> [Pos]
walk p0 =
  do
    p1 <- move p0
    guard (withinBoard p1)

    p2 <- move p1
    guard (withinBoard p2)

    p3 <- move p2
    guard (withinBoard p3)

    return p3

moveNext :: Pos -> [Pos]
moveNext p0 = do
  p1 <- move p0
  guard (withinBoard p1)
  return p1

spec :: Spec
spec = do
  it "should move 1 step" $ do
    walk (Pos 1 1) `shouldBe` foldr (\_ a -> concatMap moveNext a) [Pos 1 1] [1,2,3]
