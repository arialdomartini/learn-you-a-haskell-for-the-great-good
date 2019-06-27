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


zipWith' ::  (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = (f a b) : zipWith' f as bs

zipInTuple :: [a] -> [b] -> [(a, b)]
zipInTuple = zipWith' makeTuple
  where makeTuple :: a -> b -> (a, b)
        makeTuple a b = (a, b)

zipSum :: (Num a) => [a] -> [a] -> [a]
zipSum = zipWith' (+)

zipRepeating :: [a] -> [Int] -> [[a]]
zipRepeating = zipWith repeatElements
  where repeatElements a b = take b (repeat a)


flipArgs :: (a -> b -> c) -> (b -> a -> c)
flipArgs f = \a b -> f b a

-- since -> is right associative
flipArgs' :: (a -> b -> c) -> b -> a -> c
flipArgs' f x y = f y x

tryFlipArgs = zipWith (flipArgs (/)) [2, 3, 4] [10, 6, 20]

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs


mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f [] = []
mapIndex f xs = mapIndexRec f xs 0
  where mapIndexRec :: (a -> Int -> b) -> [a] -> Int -> [b]
        mapIndexRec f [] _ = []
        mapIndexRec f (x:xs) index = f x index : mapIndexRec f xs (index + 1)

tryMapIndex = mapIndex (\e i -> (e, i)) ["ciao", "mamma", "guarda", "come", "mi", "diverto"]


