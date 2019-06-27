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
