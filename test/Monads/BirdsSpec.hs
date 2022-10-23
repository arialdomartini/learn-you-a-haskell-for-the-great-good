module Monads.BirdsSpec where

import Test.Hspec

data Pole = Pole { left:: Int, right:: Int } deriving (Eq, Show)

type Left  = Int
type Right = Int


landLeft :: Left -> Pole -> Maybe Pole
landLeft l = walk (l, 0)

landRight :: Right -> Pole -> Maybe Pole
landRight r = walk (0, r)

banana :: Pole -> Maybe Pole
banana _ = Nothing

walk :: (Left, Right) -> Pole -> Maybe Pole
walk (l, r) pole =
  let newLeft  = left pole + l
      newRight = right pole + r
      difference = abs (newLeft - newRight) in
    if difference < 4
    then Just Pole {left = newLeft, right = newRight}
    else Nothing

spec :: Spec
spec = do
  it "falls with 4 birds on the right" $ do
    (return Pole { left = 0, right = 0 }
      >>= landLeft 1
      >>= landLeft 1
      >>= landLeft 1
      >>= landLeft 1    -- falls here
      >>= walk ((-2), 0)) -- too late
      `shouldBe` Nothing

  it "walks if never exceeds the threshold" $ do
    (return Pole { left = 0, right = 0 }
      >>= landLeft 1
      >>= landLeft 1
      >>= landLeft (-1)
      >>= landRight 2
      >>= walk (1, 1))
      `shouldBe` Just Pole { left= 2, right= 3 }


  it "slips on bananas" $ do
    (return Pole { left = 0, right = 0 }
      >>= landLeft 1
      >>= banana
      >>= landLeft (-1))
      `shouldBe` Nothing
