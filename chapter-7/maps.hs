import qualified Data.Map as Map

phoneBook :: [(String,String)]
phoneBook =
  [("charlie", "1234")
  ,("mario", "3494433")
  ,("Karl", "100100")]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey k xs = case xs of
  [] -> Nothing
  (x:xs) ->
    if k == fst x
    then Just (snd x)
    else findKey k xs

findKey' :: (Eq k) => k -> [(k, v)] -> v
--findKey' k = snd . head . filter (\x -> fst x == k)
findKey' k = snd . head . filter (\x -> fst x == k)

findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' k = foldr ff Nothing
  where ff (key,value) result = if key == k then Just value else result


tryFromList = Map.fromList someList == fromList' someList
  where someList = [(1, "john"), (2, "jasmine")]

fromList' :: (Eq k, Ord k) => [(k,v)] -> Map.Map k v
fromList' xs = foldr appendItem Map.empty xs
  where appendItem (key, value) map = Map.insert key value map

singleton' :: (Ord k) => k -> v -> Map.Map k v
singleton' k v = Map.insert k v Map.empty

map' :: (Eq k) => (v -> v') -> [(k,v)] -> [(k,v')]
map' f xs = map apply xs
  where apply (k,v) = (k, f v)

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
