module ListComprehensionSpec(spec) where

import Test.Hspec

spec :: Spec
spec = do


  it "can use list comprehensions" $ do
    let lc =     [(x * 2, y) | x <- [1..10], y <- [1..10], y `mod` 2 == 0] :: [(Int, Int)]
    let manual = buildList
    lc `shouldBe` manual



buildList :: [(Int, Int)]
buildList = build' 1 1 []


build' :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
build' x y acc =
  case (x, y) of
  (10, 10) -> maybeAdd acc x y
  (_,  10) -> build'(x+1) (1)     ( maybeAdd acc x y)
  (_, _)   -> build' (x)   (y+1)   ( maybeAdd acc x y)
  where
    maybeAdd acc' x' y' =
      if y' `mod` 2 == 0
      then acc' ++ [(x'*2, y')]
      else acc'
