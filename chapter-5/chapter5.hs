filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x then x : (filter f xs) else filter f xs

quicksortFilter :: (Ord a) => [a] -> [a]
quicksortFilter [] = []
quicksortFilter (x:xs) = quicksortFilter smaller ++ [x] ++ quicksortFilter bigger
  where smaller = filter' (\i -> i<x) xs
        bigger = filter' (\i -> i>=x) xs


-- a String is already a list
quicksortString :: String -> String
quicksortString = quicksortFilter


multiplesOf :: Int -> Int -> [Int]
multiplesOf d top = [n | n <- [top, top-1..0], n `mod` d == 0]

-- no matter the size of top, the execution is fast, because Haskell is lazy
-- and the [top, top-1..0] is not entirely visited
largestMultiple d top = head (multiplesOf d top)

largestMultiple' d top = top - top `mod` d

-- in this case, laziness doesn't help...
largestMultipleReverse d top = head (multipleReversed d top)
  where multipleReversed d top = reverse [n | n <- [0..top], n `mod` d == 0]


collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz (n * 3 + 1)

tryCollatz = collatz 68263682


longestCollatz :: Int -> Int
longestCollatz n = max' (map length (map collatz [1..n]))
  where max' xs = foldl (\acc el -> max acc el) (-1) xs  




maxInfo acc el = if snd el > snd acc then el else acc

longestCollatzWithLength :: Int -> (Int,Int)
longestCollatzWithLength n = max' (map lengthInfo (map collatzInfo [1..n]))
  where collatzInfo n = (n, collatz n)
        lengthInfo info= (fst info, length (snd info)) 
        max' xs = foldl maxInfo (0,0) xs  




foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs


-- Implementing functions with foldr

maximum' :: (Num a, Ord a) => [a] -> a
maximum' xs = foldr (\e a -> max a e) 0 xs

reverse' :: [a] -> [a]
reverse' xs = foldr (\e a -> a ++ [e]) [] xs

product' :: (Num a) => [a] -> a
product' xs = foldr (\e a -> e * a) 1 xs

filterFold' :: (a -> Bool) -> [a] -> [a]
filterFold' f xs = foldr (\e a -> if f e then e:a else a) [] xs

head' :: [a] -> a
head' = foldr1 (\e a -> e)

last' :: [a] -> a
last' = foldl1 (\a e -> e)

maximumScan :: (Ord a, Num a) => [a] -> [a]
maximumScan xs = scanl (\a e -> if e > a then e else a) 0 xs

-- This is pretty neat! Like it.
--reverseScan :: [a] -> [[a]]
reverseScan xs = scanl (\a e -> e : a) [] xs
reverseScan' xs = scanl (\a e -> (a !: e)) [] xs
  where (!:) = flip (:)
reverseScan'' xs = scanl (flip (:)) [] xs
reverseScan''' = scanl (flip (:)) []

myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr f acc xs = myScanr' f acc xs []
  where myScanr' f acc [] history = history
        myScanr' f acc (x:xs) history = myScanr' f newAcc xs newHistory
          where newAcc = (f x acc)
                newHistory = history ++ [(f x acc)]
