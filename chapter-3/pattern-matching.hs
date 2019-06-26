lucky :: Int -> String
lucky 7 = "You lucky one!"
lucky _ = "Sorry..."


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- pattern matching inside a list comprehension
sumPairs :: [(Int, Int)] -> [Int]
sumPairs pairs = [a + b | (a, b) <- pairs]

bmiCalculator :: Double -> Double -> String
bmiCalculator weight height
  | bmi <= skinny = "Eat more!"
  | bmi <= normal = "Ok"
  | bmi <- max   = "Do some workout"
  | otherwise   = "Uhm..."
  where bmi = weight / height ^ 2
        (skinny, normal, max) = (18.5, 25, 30)

getInitials :: String -> String -> String
getInitials firstname lastname = [f] ++ " " ++ [l]
  where (f:_) = firstname
        (l:_) = lastname
  
getInitials2 :: String -> String -> (Char, Char)
getInitials2 a b = (ia, ib)
  where (ia:_) = a
        (ib:_) = b


getInit :: String -> String -> (Char, Char)
getInit a b = (ia, ib)
  where (ia:_) = a
        (ib:_) = b


myMax :: (Ord a) => a -> a -> a
myMax a b
  | a > b = a
  | a == b = a
  | otherwise = b



testLet = 2 + (let x = 5 in x * 2) -- this works
-- testWhere = 2 + (x * 2 where x = 5) -- this doesn't


headPatternMatching :: [a] -> a
headPatternMatching [] = error "Empty list"
headPatternMatching (x:_) = x

headCaseExpression :: [a] -> a
headCaseExpression xs = case xs of
  [] -> error "Empty list"
  (x:_) -> x

  
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
  [] -> "empty"
  (x:[]) -> "singleton"
  (x1:x2:[]) -> "containing just 2 elements"
  (x:rest) -> show (length xs) ++ " elements long"

describeListWithWhere :: (Show a) => [a] -> String
describeListWithWhere xs = "The list is " ++ description
  where description = case xs of
          [] -> "empty"
          [a] -> "singleton"
          [a, b] -> "containing just 2 elements"
          (a:b:rest) -> show (length xs) ++ " elements long and the second element is " ++ show b
