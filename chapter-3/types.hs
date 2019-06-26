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

-- tail-recursive prefix operator that perfmorms a multiplication
(>*) :: Int -> Int -> Int
(>*) a 0 = 0
(>*) a 1 = a
(>*) a b = a + (>*) a (b-1)

-- :t elem
-- could be:
-- (Eq a) => a -> [a] -> Bool
-- actually, it is:
-- (Foldable t, Eq a) => a -> t a -> Bool

tryRead = (read "4" :: Integer) * 10 -- the explicit type annotation is needed otherwise an exception is raised
tryRead2 = 0 : read "[1,2,3,4]" ++ read "[5,6]" -- here the type is inferred

-- To know the typeclasses implemented by a type use :info
-- *Main> :info Int
-- data Int = GHC.Types.I# GHC.Prim.Int#   -- Defined in ‘GHC.Types’
-- instance Eq Int -- Defined in ‘GHC.Classes’
-- instance Ord Int -- Defined in ‘GHC.Classes’
-- instance Show Int -- Defined in ‘GHC.Show’
-- instance Read Int -- Defined in ‘GHC.Read’
-- instance Enum Int -- Defined in ‘GHC.Enum’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Real Int -- Defined in ‘GHC.Real’
-- instance Bounded Int -- Defined in ‘GHC.Enum’
-- instance Integral Int -- Defined in ‘GHC.Real’



-- a number is a polymorphic type

-- tryPolymorphic :: Num a => a
tryPolymorphic = (5 :: Float) + 4.2

-- *Main> :t 5
-- 5 :: Num p => p
-- *Main> :t (5 :: Integer)
-- (5 :: Integer) :: Integer
-- *Main> :t (5 :: Float)
-- (5 :: Float) :: Float

