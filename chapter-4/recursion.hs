maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot calculate maximum on an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' 1 element = [element]
replicate' n element = element : (replicate' (n - 1) element)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : ( take (n-1) xs )
