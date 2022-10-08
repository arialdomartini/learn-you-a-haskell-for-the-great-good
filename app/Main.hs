module Main(main) where

import qualified Data.Map as Map
import Control.Monad(forever)
import Data.Char (toUpper)
import Control.Exception (handle)
import System.IO (withFile, IOMode (ReadMode))
import GHC.IO.Handle
import GHC.Show (intToDigit)
import System.Environment (getArgs)
import qualified FastCopy as Fast(copyFile)

choices :: Map.Map String ([String] -> IO ())
choices = Map.fromList [
  ("help", printHelp),
  ("for-ever-print", sequenceForEverPrint),
  ("salute", salute),
  ("read-input", readInput),  -- run it with (echo 3 && cat README.md) | make run
  ("read-input-with-getContents", readInputWithGetContents),
  ("only-short-lines", shortLines),
  ("read-a-file", readAFile),
  ("copy-a-file", copyFile),
  ("fast-cp", Fast.copyFile),
  ("delete-line-from-file", deleteLine),
  ("print-args", printArgs)]


main :: IO ()
main = do
  args <- getArgs
  if null args
    then printHelp args
    else do let (command : t) = args
            dispatch command t

dispatch :: String -> [String] -> IO ()
dispatch k args = do
  let choice = Map.lookup k choices
  case choice of
    Nothing -> putStrLn "No command found"
    Just command -> command args

printHelp :: [a] -> IO ()
printHelp _ = do sequence_ $ Map.mapWithKey printCommand choices

printCommand :: String ->  ([String] -> IO ()) -> IO ()
printCommand s c = putStrLn s


printArgs :: [a] -> IO ()
printArgs _ = do
  args <- getArgs
  print args

deleteLine :: [a] -> IO ()
deleteLine _ = do
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

copyFile :: [a] -> IO ()
copyFile _ = do
  from <- getLine
  to <- getLine
  putStrLn $ from ++ " -> " ++ to
  content <- readFile from
  writeFile to content
  return ()

readAFile :: [a] -> IO ()
readAFile _ = do
  withFile "README.md" ReadMode $
    \h -> do
       content <- hGetContents h
       putStr content

readInputWithGetContents :: [a] -> IO ()
readInputWithGetContents _ = forever $ do
  v <- getContents
  putStrLn $ fmap toUpper v

shortLines :: [a] -> IO ()
shortLines _ = interact $ onlyTakeShortLines

onlyTakeShortLines :: String -> String
onlyTakeShortLines = unlines . fmap (\s -> if length s > 10 then "too long" else s) . lines

readInput :: [a] -> IO()
readInput _ =
  forever $ do
    line <- getLine
    putStrLn $ map toUpper line


sequenceForEverPrint :: [a] -> IO ()
sequenceForEverPrint _ = do
  sequence_ forEverPrint


forEverPrint :: [IO ()]
forEverPrint = repeat singlePrint

singlePrint :: IO ()
singlePrint = do
    line <- getLine
    putStrLn $ reverse line


salute :: [a] -> IO ()
salute _ =
  do putStrLn "Hello World"
     putStrLn "Hello World. What's your name?"
     name <- getLine
     putStrLn $ "Hola, " ++ name
     putStrLn $ tellFortune name


tellFortune name = name ++ " you will be very lucky"
