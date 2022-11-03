module Monads.WriterBanditsSpec where

import Test.Hspec
import Data.Monoid (Sum(Sum))

isBigGang :: Int -> (Bool, String)
isBigGang n = (n > 9, "compared gang size of " <> show n <> " to 9")

isBigGangList :: Int -> (Bool, [String])
isBigGangList n = (n > 9, pure $ "compared gang size of " <> show n <> " to 9")


applyLog :: Semigroup l => (a, l) -> (a -> (b, l)) -> (b, l)
applyLog (v, l') f =
  let (v'', l'') = f v in
    (v'', l' <> l'')


type Food = String
type Drink = String
type Price = Sum Int

addDrink :: Food -> (Drink, Price)
addDrink "beans" = ("whiskey", Sum 10)
addDrink "pasta" = ("wine", Sum 5)
addDrink "bisquits" = ("milk", Sum 2)
addDrink _ = ("beer", Sum 2)






spec :: Spec
spec = do
  it "compares gang size" $ do
    isBigGang 9 `shouldBe`  (False, "compared gang size of 9 to 9")
    isBigGang 30 `shouldBe` (True, "compared gang size of 30 to 9")
    isBigGang 3 `shouldBe`  (False, "compared gang size of 3 to 9")

  it "feeds a tuple to a function returning a tuple" $ do
    let smallGang = (3, pure "smallish gang") in
      smallGang `applyLog` isBigGangList `shouldBe` (False, ["smallish gang", "compared gang size of 3 to 9"])

  it "uses applyLog to calculate total cost" $ do
    ("beans", Sum 100) `applyLog` addDrink `shouldBe` ("whiskey", Sum (100 + 10))
