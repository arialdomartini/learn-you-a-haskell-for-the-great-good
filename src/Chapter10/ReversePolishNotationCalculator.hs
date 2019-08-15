module Chapter10.ReversePolishNotationCalculator where

--data Operazione = Plus | Minus
data Item = Numero Int | Plus

rpn :: String -> Int
rpn = calculate . tokenize

tokenize = words

calculate :: [String] -> Int
calculate = head . foldl accumulate []

accumulate :: [Int] -> String -> [Int]
accumulate (a:b:as) "+" = (a + b) : as
accumulate (a:b:as) "*" = (a * b) : as
accumulate acc n = (read n):acc
