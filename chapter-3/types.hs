isLowerCase :: Char -> Bool
isLowerCase c = c `elem` ['a'..'z'] ++ [' ']
removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase [] = []
removeNonUpperCase (x:xs) = (if isLowerCase x then x:removeNonUpperCase xs else removeNonUpperCase xs)

-- with a list comprehension
removeNonUpperCase2 :: [Char] -> [Char]
removeNonUpperCase2 xs = [x | x <- xs, isLowerCase x]


addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- *Main> :t addThree
-- addThree :: Int -> Int -> Int -> Int


-- small numbers
factorial :: Int -> Int
factorial x = product [1..x]
-- big numbers
factorialBig :: Integer -> Integer
factorialBig x = product [1..x]

-- *Main> factorial 24
-- -7835185981329244160
-- *Main> factorialBig 24
-- 620448401733239439360000


-- fst implemented with pattern matching
myFst :: (a, b) -> a
myFst (a,b) = a

-- :t (==)
-- (Eq a) => a -> a -> Bool

-- prefix operator redefining equality
(>>>=) :: (Eq a) => a -> a -> Bool
(>>>=) = (==)

tryAreEqual a b = a >>>= b

