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
  
