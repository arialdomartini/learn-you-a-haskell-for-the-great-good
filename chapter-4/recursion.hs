maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot calculate maximum on an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
