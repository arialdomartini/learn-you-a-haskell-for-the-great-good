module Main(main) where

import qualified Data.Map as Map
import Control.Monad(forever)

choices = Map.fromList [
  (1, ("forEverPrint", sequenceForEverPrint)),
  (2, ("salute", salute))] :: Map.Map Int (String, IO ())

main :: IO ()
main = forever menu


menu :: IO ()
menu = do
  putStrLn "1 forEverPrint"
  putStrLn "2 salute"
  choice <- getLine
  let choiceN = (read choice) :: Int
  let f = Map.lookup choiceN choices
  case f of
    Nothing -> putStrLn "Error: unknown choice."
    Just f -> do
      snd f
      putStrLn "================"


sequenceForEverPrint :: IO ()
sequenceForEverPrint = do
  sequence_ forEverPrint


forEverPrint :: [IO ()]
forEverPrint = repeat singlePrint

singlePrint :: IO ()
singlePrint = do
    line <- getLine
    putStrLn $ reverse line


salute :: IO ()
salute =
  do putStrLn "Hello World"
     putStrLn "Hello World. What's your name?"
     name <- getLine
     putStrLn $ "Hola, " ++ name
     putStrLn $ tellFortune name


tellFortune name = name ++ " you will be very lucky"
