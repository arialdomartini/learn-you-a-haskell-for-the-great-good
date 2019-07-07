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
