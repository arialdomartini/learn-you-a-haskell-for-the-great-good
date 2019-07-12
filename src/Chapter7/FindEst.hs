module Chapter7.FindEst (
  gimmefive
) where

import Data.List
import Data.Function

gimmefive = 5

findEst :: (a -> Bool) -> [a] -> [a]
findEst f xs = getLongest separate
  where getLongest (left,right) = if length left == 1 then right else left
        separate = foldr checkCondition ([],[]) xs
          where checkCondition e (yes,no) = if f e then (e:yes, no) else (yes, e:no)

findEst' f xs = head $ filter (\x -> length x > 1) [left, right]
  where left = filter f xs
        right = filter (not . f) xs


findEst'' f xs = foldr ff [] xs
  where ff e [] = [e]
        ff e xx@(x:xs) = if f e == f x then e:xx else xx

--findEnd''' :: (Ord a) => (a -> Bool) -> [a] -> [a]
findEst''' f xs = maximumBy (compare `on` length) [fst s, snd s]
  where s = partition f xs


