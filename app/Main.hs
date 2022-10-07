module Main(main) where

import qualified Data.Map as Map
import Control.Monad(forever)
import Data.Char (toUpper)
import Control.Exception (handle)
import System.IO (withFile, IOMode (ReadMode))
import GHC.IO.Handle
import GHC.Show (intToDigit)
import System.Environment (getArgs)

choices :: Map.Map Int (String, IO ())
choices = Map.fromList [
  (1, ("forEverPrint", sequenceForEverPrint)),
  (2, ("salute", salute)),
  (3, ("read input", readInput)),  -- run it with (echo 3 && cat README.md) | make run
  (4, ("read input with getContents", readInputWithGetContents)),
  (5, ("only short lines", shortLines)),
  (6, ("read a file", readAFile)),
  (7, ("copy a file", copyFile)),
  (8, ("delete line from file", deleteLine)),
  (9, ("print args", printArgs))
  ]

printArgs :: IO ()
printArgs = do
  args <- getArgs
  print args

deleteLine :: IO ()
deleteLine = do
  let fileName = "README.md"
  content <- readFile fileName
  let numberedLines = (zipWith (,) (fmap show [1..]). lines) content
  let contentLines = unlines $ fmap (\(i,c) -> (show i) ++ ") " ++ c) numberedLines
  putStrLn contentLines
  lineToDelete <-getLine
  let lineNumber = (read lineToDelete) :: Int
  let filtered = filter (\(i,_) -> i /= lineToDelete) numberedLines
  let outFileName = "deleted_" ++ fileName
  writeFile outFileName (unlines (fmap snd filtered))
  putStrLn $ "Have a look to " ++ outFileName

copyFile :: IO ()
copyFile = do
  from <- getLine
  to <- getLine
  putStrLn $ from ++ " -> " ++ to
  content <- readFile from
  writeFile to content
  return ()

readAFile :: IO ()
readAFile = do
  withFile "README.md" ReadMode $
    \h -> do
       content <- hGetContents h
       putStr content

readInputWithGetContents :: IO ()
readInputWithGetContents = forever $ do
  v <- getContents
  putStrLn $ fmap toUpper v

shortLines :: IO ()
shortLines = interact $ onlyTakeShortLines

onlyTakeShortLines :: String -> String
onlyTakeShortLines = unlines . fmap (\s -> if length s > 10 then "too long" else s) . lines

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
