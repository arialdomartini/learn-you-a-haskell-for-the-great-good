import Data.List
-- import Data.List (nub, sort)
-- import Data.List hiding (sort)

-- numUniques = length . nub

-- import qualified Data.List (nub)
-- numUniques = length . Data.List.nub

--import qualified Data.List as M
-- numUniques = length . M.nub


-- without the import statement, this function doesn't compile,
-- as nub is defined in a module different than Prelude
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- :browse Data.List from ghci lists the exported functions


-- Data.List
-- intersperse
tryIntersperse = intersperse '.' "GIMP" == "G.I.M.P"

myIntersperse :: a -> [a] -> [a]
myIntersperse separator list = case list of
  [] -> []
  [a] -> [a]
  (x:xs) -> x : separator : (myIntersperse separator xs)

-- intercalate
tryIntercalate = intercalate " " ["hello", "cruel", "world"] == "hello cruel world"

myIntercalate :: [a] -> [[a]] -> [a]
myIntercalate separator lists = case lists of
  [] -> []
  [a] -> a
  (x:xs) -> x ++ separator ++ myIntercalate separator xs

-- transpose
tryTranspose = transpose
  [ [1,2,3],
    [10,20,30],
    [100,200,300] ]
  ==
  [ [1,10,100],
    [2,20,200],
    [3,30,300] ]


--myTranspose :: [[a]] -> [[a]]
--myTranspose lists = myTranspose' lists []

--myTranspose' [] = []
--myTranspose' (x:xs) res = myTranspose' xs
-- [1,2,3] -> [[1], [2], [3]]
--transp :: [a] -> [[a]]
transp [] = []
transp (x:xs) = [x] : transp xs

-- accum [[1], [2], [3]] -> [[10], [20], [30]] ->  [[1,10], [2,20], [3,30]]
-- accum :: [[a]] -> [[a]] -> [[a]]
-- accum [] _ = []
-- accum _ [] = []
-- accum (x:xs) (y:ys) =

-- [ [1,2,3], [10,20,30], [100,200,300] ]
-- [ [1,10,100], [2,20,200], [3,30,300] ]

toListOfLists x = map (\e -> [e]) x

-- done! Not that simple...

transpose' xs = foldr (\e a -> aggregate e a) [] xs
  where aggregate [] _ = []
        aggregate (x:xs) [] = [x] : (aggregate xs [])
        aggregate (x:xs) (y:ys) = (x:y) : aggregate xs ys

tryTranspose' = transpose' [ [1,2,3],    [10,20,30], [100,200,300] ] ==
                           [ [1,10,100], [2,20,200], [3,30,300] ] 
