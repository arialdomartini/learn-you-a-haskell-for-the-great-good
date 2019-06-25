isLowerCase :: Char -> Bool
isLowerCase c = c `elem` ['a'..'z'] ++ [' ']
removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase [] = []
removeNonUpperCase (x:xs) = (if isLowerCase x then x:removeNonUpperCase xs else removeNonUpperCase xs)

-- with a list comprehension
removeNonUpperCase2 :: [Char] -> [Char]
removeNonUpperCase2 xs = [x | x <- xs, isLowerCase x]
