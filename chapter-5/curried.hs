flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f x y = g
  where g = f y x

sub' x y = x - y

s1 = flip' sub'
s2 = flip'' sub'


