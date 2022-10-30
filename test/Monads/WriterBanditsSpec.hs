module Monads.WriterBanditsSpec where

import Test.Hspec

isBigGang :: Int -> (Bool, String)
isBigGang n = (n > 9, "compared gang size of " <> show n <> " to 9")

applyLog :: (Int, String) -> (Int -> (Bool, String)) -> (Bool, String)
applyLog (v, l') f =
  let (v'', l'') = f v in
    (v'', l' <> l'')


spec :: Spec
spec = do
  it "compares gang size" $ do
    isBigGang 9 `shouldBe`  (False, "compared gang size of 9 to 9")
    isBigGang 30 `shouldBe` (True, "compared gang size of 30 to 9")
    isBigGang 3 `shouldBe`  (False, "compared gang size of 3 to 9")

  it "feeds a tuple to a function returning a tuple" $ do
    let smallGang = (3, "smallish gang,") in
      applyLog smallGang isBigGang `shouldBe` (False, "smallish gang,compared gang size of 3 to 9")
