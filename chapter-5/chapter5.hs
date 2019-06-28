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
