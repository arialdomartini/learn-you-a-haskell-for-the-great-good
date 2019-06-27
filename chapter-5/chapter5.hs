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
