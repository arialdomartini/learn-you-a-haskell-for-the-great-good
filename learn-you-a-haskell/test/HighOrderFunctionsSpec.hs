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

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = (f a b) : zipWith' f as bs

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f a b = f b a

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

  it "defines zipWith as high-order function" $ do
    let as = [1,2,3] :: [Int]
    let bs = [2,3,4]
    (zipWith' (+) as bs) `shouldBe` (zipWith (+) as bs)

  it "flips parameters of a function" $ do
    let nameSurname n s = n ++ " " ++ s
    let surnameName  = flip' nameSurname
    (surnameName "Mario" "Cioni") `shouldBe` "Cioni Mario"
