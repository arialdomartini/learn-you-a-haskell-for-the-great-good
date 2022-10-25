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

spec :: Spec
spec = do
  it "should move 1 step" $ do
    walk (Pos 1 1) `shouldBe` [Pos 7 4,Pos 7 2,Pos 3 4,Pos 3 2,Pos 6 5,Pos 6 1,Pos 4 5,Pos 4 1,Pos 7 2,Pos 3 2,Pos 6 3,Pos 4 3,Pos 3 4,Pos 3 2,Pos 2 5,Pos 2 1,Pos 3 2,Pos 2 3,Pos 6 5,Pos 6 3,Pos 2 5,Pos 2 3,Pos 5 6,Pos 5 2,Pos 3 6,Pos 3 2,Pos 4 5,Pos 4 3,Pos 3 6,Pos 3 2,Pos 1 6,Pos 1 2,Pos 6 5,Pos 6 3,Pos 2 5,Pos 2 3,Pos 5 6,Pos 5 2,Pos 3 6,Pos 3 2,Pos 6 3,Pos 6 1,Pos 2 3,Pos 2 1,Pos 5 4,Pos 3 4,Pos 5 6,Pos 5 4,Pos 1 6,Pos 1 4,Pos 4 7,Pos 4 3,Pos 2 7,Pos 2 3,Pos 5 2,Pos 1 2,Pos 4 3,Pos 2 3,Pos 3 6,Pos 3 4,Pos 2 7,Pos 2 3,Pos 3 2,Pos 2 3]
