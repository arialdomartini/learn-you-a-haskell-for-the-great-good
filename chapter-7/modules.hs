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


concat' :: [String] -> String
concat' [] = ""
concat' (x:xs) = x ++ concat' xs

tryConcat1 = concat ["hello", "happy", "world"] == "hellohappyworld"
tryConcat2 = concat [ [1,2,3], [10,20,30], [100]] == [ 1,2,3,10,20,30,100]

concatWithFold :: [String] -> String
concatWithFold = foldr (++) ""


tryConcatMap = concatMap (replicate 4)  [1..3] == [1,1,1,1, 2,2,2,2, 3,3,3,3]
tryConcatMap' = concatMap' (replicate 4)  [1..3] == [1,1,1,1, 2,2,2,2, 3,3,3,3]

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat . map f


and' xs = case xs of
  [] -> True
  (x:xs) -> if x then and' xs else False

tryAnd1 = and' [True,True,True,True] == True
tryAnd2 = and' [True,False,True,True] == False

-- and implemented with foldr
andFold xs = foldr (&&) True xs
tryAndFold = andFold [True,False,True,True] == False

all' f = foldr (\e a -> f e && a) True
any' f = foldr (\e a -> f e || a) False


tryIterate = take 8 (iterate'' (*2) 1) == [1,2,4,8,16,32,64,128]

iterate'' :: (a -> a) -> a -> [a]
iterate'' f init = init : (iterate'' f (f init))

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xx@(x:xs) =
  if n == 0
  then ([], xx)
  else addFst x (splitAt' (n-1) xs)
    
  where addFst x (f,s) = (x:f, s)

-- very convoluted
splitAtFold n xs = fst $ foldr funcc accInit xs
  where accInit = (([], []), length xs)
        funcc:: a -> (([a], [a]), Int) -> (([a], [a]), Int)
        funcc e a =
          if idx > n
          then
            make left (e:right) (idx-1)
          else
            make (e:left) right (idx-1)
          where idx = snd a
                pair = fst a
                make a b i = ((a, b), i)
                left = fst pair
                right = snd pair
          

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] =[]
takeWhile' f (x:xs) = if f x then x:(takeWhile' f xs) else []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold f xs = foldr getNext [] xs
  where getNext e a = if f e then e:a else []


dropWhileFold :: (a -> Bool) -> [a] -> [a]

dropWhileFold f = foldl dropNext []
  where dropNext a e = case a of
          [] -> if f e then [] else [e]
          _ -> a ++ [e]

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f xx@(x:xs) = if f x then dropWhile' f xs else xx                        

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' f xs = spanIter xs ([], [])
  where spanIter xx@(x:xs) acc = if f x then spanIter xs ((fst acc) ++ [x], []) else (fst acc, xx)
        spanIter [] acc = acc

break' p = span (not . p)

group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' xs = groupIter xs [] []
  where groupIter :: (Eq a) => [a] -> [[a]] -> [a] -> [[a]]
        groupIter [] acc group = acc ++ [group]
        groupIter (x:xs) acc [] = groupIter xs acc [x]
        groupIter (x:xs) acc group = if x == head group
          then groupIter xs acc (x:group)
          else groupIter xs (acc ++ [group]) [x]


inits' :: [a] -> [[a]]
inits' = foldl append [[]]
  where append :: [[a]] -> a -> [[a]]
        append [] e = [[e]]
        append acc e = acc ++ [last acc ++ [e]]

tails' :: [a] -> [[a]]
tails' xs = foldr append [] xs
  where append :: a -> [[a]] -> [[a]]
        append e [] = [[e]]
        append e acc = (e : (head acc)) : acc

group'' xs = reverse $ groupRec xs []
  where
    groupRec [] acc = acc
    groupRec (x:xs) acc = groupRec xs (push' x acc)
      where push' e [] = [[e]]
            push' e aa@(a:as) = if e == head a then (e:a):as else [e]:aa

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (a:as) = if x == a then isPrefixOf' xs as else False

isPrefixOf'' xs as = foldl comp True pairs
  where pairs = zip xs as
        comp result (left,right) = result && left == right

isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' a x = isPrefixOf'' (reverse a) (reverse x)

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f xs = partitionRec f xs ([],[])
  where partitionRec _ [] acc = acc
        partitionRec f (x:xs) (left, right) =
          if f x then (x:left, right)
          else (left, x:right)

find' :: (a -> Bool) -> [a] -> Maybe a
find' f = foldl comp Nothing
  where comp result e = if f e then Just e else result

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' el xs = elemIndexRec el xs 0
  where elemIndexRec el [] _ = Nothing
        elemIndexRec el (x:xs) index = if el == x then Just index else elemIndexRec el xs (index+1)

elemIndex'' el xs = snd $ foldl comp (0, Nothing) xs
  where comp (index, result) e = if el == e then (index, Just index) else (index+1, result)

elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' e xs = iter e xs 0
  where iter e [] _ = []
        iter e (x:xs) index = if e == x then index : rest else rest
          where rest = iter e xs (index+1)

elemIndices'' :: (Eq a) => a -> [a] -> [Int]
elemIndices'' e xs = snd $ foldr f (0, []) xs
  where f el (index, acc) = if e == el then (index+1, acc++[index]) else (index+1, acc)

findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' f xs = recur f xs 0
  where recur _ [] _ = Nothing
        recur f (x:xs) index = if f x then Just index else recur f xs (index+1)

findIndex'' :: (a -> Bool) -> [a] -> Maybe Int
findIndex'' f xs = snd $ foldr comp (0, Nothing) xs
  where comp e (index, acc) = if f e then (index+1, Just index) else (index+1, acc)

findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' f xs = recur f xs 0
  where recur f xs index = case xs of
          [] -> []
          (x:xs) -> if f x then index : continue else continue
            where continue = recur f xs (index + 1)

findIndices'' f xs = reverse $ snd $ foldr comp (0, []) xs
  where comp e (index, acc) = if f e then (index+1, index:acc) else (index+1, acc)

