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

(|>) :: a -> (a -> b) -> b
a |> f = f a

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (h:t) = f h : map' f t

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : recurse
  | otherwise =     recurse
  where recurse = filter' p xs

collatz :: Int -> [Int]
collatz n =
  let r = (step n) in
  case r of
    1         -> [1]
    _         -> r : (collatz r)
  where
    step i
     | even i    = i `div` 2
     | True      = 3 * i + 1



deconstruct :: [(a,a)] -> [a]
deconstruct xs = foldr (\(x, y) a -> x: y : a) [] xs

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

  it "F#'s pipe'" $ do
    (12::Int) |> (1 +) `shouldBe` 13


  it "maps a function to a list" $ do
    (map' (*2) ([1,2,3] :: [Int])) `shouldBe` [2,4,6]

  it "maps to lists of lists" $ do
    (map' (map' (++"!") ) [["hey", "Joe"], ["Stop"]]) `shouldBe` [["hey!", "Joe!"], ["Stop!"]]

  it "bimap" $ do
    let bimap f = map (map f)
    (bimap (++"!")  [["hey", "Joe"], ["Stop"]]) `shouldBe` [["hey!", "Joe!"], ["Stop!"]]

  it "list filter" $ do
    (filter (\i -> length i > 5) ["hey", "123456"]) `shouldBe` ["123456"]

  it "composes filters" $ do
    let f1 = (>(10::Int))
    let f2 = (<(50 :: Int))
    let fx ^+ fy = \v -> (fx v) && (fy v)
    (filter (f1 ^+ f2) ([1, 12, 49, 51] :: [Int])) `shouldBe` ([12, 49] :: [Int])

  it "generates a collatz series" $ do
    (last (collatz 100)) `shouldBe` 1

  it "deconstruts lists of tuples" $ do
    (deconstruct [(1,2), (3,4), (5,6)]) `shouldBe` ([1,2,3,4,5,6]::[Int])
