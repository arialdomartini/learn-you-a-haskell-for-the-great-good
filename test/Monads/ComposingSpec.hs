module Monads.ComposingSpec where

import Test.Hspec
import Control.Monad ((<=<))

(<=<<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=<< g = \a ->
  let r = g a in
    r >>= f


type KnightPosition = (Int, Int)

withinBorders :: Int -> Bool
withinBorders n = n <=8 && n >=1

isValid :: KnightPosition -> Bool
isValid (x, y) = withinBorders x && withinBorders y

move :: KnightPosition -> [KnightPosition]
move (x, y) =
  filter isValid
    [(x+2, y+1)
    ,(x+1, y+2)

    ,(x+2, y-1)
    ,(x+1, y-2)

    ,(x-2, y+1)
    ,(x-1, y+2)

    ,(x-2, y-1)
    ,(x-1, y-2)]

g' :: String -> Maybe Int
g' s = Just (length s)

f' :: Int -> Maybe Int
f' n = Just (n * 2)

inMany :: Int -> KnightPosition -> [KnightPosition]
inMany n = foldr (<=<) return (replicate n move)


spec :: Spec
spec = do
  it "combine monadic functions" $ do
    (f' <=<< g') "hey!" `shouldBe` (f' <=< g') "hey!"

  it "moves a knight" $ do
    (return (4,4) >>= move >>= move) `shouldBe`  [(8,6),(7,7),(8,4),(7,3),(4,6),(5,7),(4,4),(5,3),(7,7),(6,8),(7,5),(6,4),(3,7),(4,8),(3,5),(4,4),(8,4),(7,5),(8,2),(7,1),(4,4),(5,5),(4,2),(5,1),(7,3),(6,4),(7,1),(3,3),(4,4),(3,1),(4,6),(3,7),(4,4),(3,3),(1,7),(1,3),(5,7),(4,8),(5,5),(4,4),(1,7),(2,8),(1,5),(2,4),(4,4),(3,5),(4,2),(3,1),(1,5),(1,1),(5,3),(4,4),(5,1),(1,3),(2,4),(1,1)]

  it "move a knight, using monadic function composition" $ do
    inMany 2 (4,4) `shouldBe` [(8,6),(7,7),(8,4),(7,3),(4,6),(5,7),(4,4),(5,3),(7,7),(6,8),(7,5),(6,4),(3,7),(4,8),(3,5),(4,4),(8,4),(7,5),(8,2),(7,1),(4,4),(5,5),(4,2),(5,1),(7,3),(6,4),(7,1),(3,3),(4,4),(3,1),(4,6),(3,7),(4,4),(3,3),(1,7),(1,3),(5,7),(4,8),(5,5),(4,4),(1,7),(2,8),(1,5),(2,4),(4,4),(3,5),(4,2),(3,1),(1,5),(1,1),(5,3),(4,4),(5,1),(1,3),(2,4),(1,1)]
