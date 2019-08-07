module Chapter9.Reverse where

main :: IO ()
main = do
  word <- getLine
  if null word
    then return ()
    else do let rev = reverse' word 
            putStrLn $ rev
            main

reverse' :: String -> String
reverse' = foldl f []
  where f acc x = x : acc
