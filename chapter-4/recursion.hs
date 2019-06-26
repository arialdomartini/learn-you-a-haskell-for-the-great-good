-- a convoluted max
getMax :: (Ord a) => [a] -> a
getMax xs = case xs of
  [] -> error "Empty list"
  xs@(x:_) -> getMaxRec xs x
    where
      getMaxRec xss maxValue = case xss of
        [] -> maxValue
        (x:rest) -> getMaxRec rest (biggest x maxValue)
        where
          biggest a b = if a > b then a else b

-- a simpler implementation
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot calculate maximum on an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

anotherMax :: (Ord a) => [a] -> a
anotherMax [x] = x
anotherMax (x:tail)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = anotherMax tail

simplerMax :: (Ord a) => [a] -> a
simplerMax [] = error "Empty list"
simplerMax [a] = a
simplerMax (x:xs) = max x (simplerMax xs)

-- interestingly, to perform the pattern matching of n on 0, n requires Eq n
repeatRec :: (Num n, Eq n) => a -> n -> [a]
repeatRec _ 0 = []
repeatRec a n = a : (repeatRec a (n - 1))

-- otherwise, with Ord
-- if the type declaration is removed, Haskell is able to infer it
-- *Main> :t repeatRec2
-- repeatRec2 :: (Ord t1, Num t1) => t2 -> t1 -> [t2]
-- impressive
repeatRec2 :: (Num n, Ord n) => a -> n -> [a]
repeatRec2 a n
  | n >= 0 = a : repeatRec2 a (n - 1)
  | otherwise = []


takeRec :: (Num n, Eq n) => n -> [a] -> [a]
takeRec _ [] = []
takeRec 0 xs = []
takeRec n (x:xs) = x : takeRec (n - 1) xs

takeGuard n xss@(x:xs)
  | n == 0 = []
  | xss == [] = []
  | otherwise = x : takeGuard (n-1) xs


replicate' :: Int -> a -> [a]
replicate' 1 element = [element]
replicate' n element = element : (replicate' (n - 1) element)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : ( take (n-1) xs )

take2' :: Int -> [a] -> [a]
take2' n xs
  | length xs == 0  = []
  | n == 0 = []
  | otherwise = head(xs) : take2' (n-1) (tail xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = (a,b) : zip as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = elem' a xs


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  quicksort' [e | e <- xs, e <= x] ++ [x] ++ quicksort' [e | e <- xs, e > x]

quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = quicksort'' (filter (\e -> e <= x) xs) ++ [x] ++ quicksort'' (filter (\e -> e > x) xs)
