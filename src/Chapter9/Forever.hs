module Chapter9.Forever where

import Control.Monad

main = forever $ do
  putStr "Tell me a number: "
  input <- getLine
  let number = read input :: Int
  putStr "I will double it for you: "
  print (number * 2)
