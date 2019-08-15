module Chapter10.ReversePolishNotationCalculator where

--data Operazione = Plus | Minus
data Item = Numero Int | Plus


calculate :: [Item] -> Int
calculate = head . foldl accumulate []

accumulate acc (Numero n) = n : acc
accumulate (f:s:as) Plus = (f + s):as

