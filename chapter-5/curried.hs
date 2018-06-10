flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f x y = g
  where g = f y x

sub' x y = x - y

s1 = flip' sub'
s2 = flip'' sub'

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10/)

useDivideByTen = divideByTen 40 -- should be 4.0
useDivideTenBy = divideTenBy 40 -- should be 2.5

