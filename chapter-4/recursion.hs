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


