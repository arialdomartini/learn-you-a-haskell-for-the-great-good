module HighOrderFunctionsSpec where

import Test.Hspec

mult3 :: Num a => a -> (a -> (a -> a))
mult3 x y z = x * y * z

divideBy10 :: Fractional a => a -> a
divideBy10 = (/10)

isAlpha :: Char -> Bool
isAlpha = (`elem` ['a'..'z'])

twice :: (a -> a) -> (a -> a)
twice f a = f (f a)

spec :: Spec
spec = do
  it "implicitly curries functions" $ do
    mult3 (2::Int) 3 4 `shouldBe` (((mult3 2) 3) 4)

  it "supports sections for infix functions" $ do
    (divideBy10 (120::Double)) `shouldBe` (120 / 10)

  it "support sections for prefix functions" $ do
    (isAlpha 'a') `shouldBe` True
    (isAlpha '.') `shouldBe` False

  it "applies a function twice" $ do
    let f x = x ++ "!"
        ff = twice f
    (ff "hey") `shouldBe` "hey!!"
    ((twice (*2)) 3) `shouldBe` (3 * 2 * 2 :: Int)
