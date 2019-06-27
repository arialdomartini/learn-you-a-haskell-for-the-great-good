applyNTimes :: (Eq n, Num n) => n -> (a -> a) -> (a -> a)
applyNTimes 1 f x = f x
applyNTimes n f x = applyNTimes (n-1) f (f x)

applyTwice :: (a -> a) -> (a -> a)
applyTwice f x = f ( f x)

double x = 2 * x
twiceDouble = applyTwice double

example = twiceDouble 3 -- should be 12

applyTwice' f = f . f
example' = applyTwice' (\x -> 2 * x) 3


zipWith' :: [a] -> [b] -> (a -> b -> c) -> [c]
zipWith' [] _ _ = []
zipWith' _ [] _ = []
zipWith' (a:as) (b:bs) f = (f a b) : zipWith' as bs f
