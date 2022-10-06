module Main(main) where

import qualified Data.Map as Map
import Control.Monad(forever)
import Data.Char (toUpper)

choices :: Map.Map Int (String, IO ())
choices = Map.fromList [
  (1, ("forEverPrint", sequenceForEverPrint)),
  (2, ("salute", salute)),
  (3, ("read input", readInput)),  -- run it with (echo 3 && cat README.md) | make run
  (4, ("read input with getContents", readInputWithGetContents))]

readInputWithGetContents :: IO ()
readInputWithGetContents = forever $ do
  v <- getContents
  putStrLn $ fmap toUpper v



main :: IO ()
main = forever menu

readInput :: IO()
readInput =
  forever $ do
    line <- getLine
    putStrLn $ map toUpper line

menu :: IO ()
menu = do
  _ <- sequence $ Map.mapWithKey execCommand choices
  choice <- getLine
  let choiceN = (read choice) :: Int
  let f = Map.lookup choiceN choices
  case f of
    Nothing -> putStrLn "Error: unknown choice."
    Just f -> do
      snd f
      putStrLn "================"

execCommand :: Int -> (String, IO ()) -> IO ()
execCommand k (s, c) = putStrLn $ show k ++ ")" ++ " " ++ s


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
