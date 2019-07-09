divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10/)

useDivideByTen = divideByTen 40 -- should be 4.0
useDivideTenBy = divideTenBy 40 -- should be 2.5


-- Applying sections on non-infix partially applied functions

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f x y = g
  where g = f y x

sub' x y = x - y

s1 = flip' sub'
s2 = flip'' sub'


myDivide :: (Floating a) => a -> a -> a
myDivide a b = a / b

myDivideByTenFlipped = flip' myDivide
myDivideByTen a = myDivideByTenFlipped 10
useMyDivideByTen = myDivideByTen 40 -- should be 4.0

isAlphaNumeric :: String -> Bool
isAlphaNumeric xs = all (\c -> c `elem` alphaNumericChars) xs
  where alphaNumericChars = ' ' : ['a'..'z'] ++ ['A'..'Z']
