module Chapter9.Reverse where

import Control.Monad

main :: IO ()
main = do
  word <- getLine
  when (not $ null word) $ do
    putStrLn $ reverse' word
    main

reverse' :: String -> String
reverse' = foldl f []
  where f acc x = x : acc
