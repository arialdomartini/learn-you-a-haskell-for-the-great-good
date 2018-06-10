applyTwice :: (a -> a) -> (a -> a)
applyTwice f x = f ( f x)

double x = 2 * x
twiceDouble = applyTwice double

example = twiceDouble 3 -- should be 12

applyTwice' f = f . f
example' = applyTwice' (\x -> 2 * x) 3
