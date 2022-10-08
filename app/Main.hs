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

choices :: Map.Map String (IO ())
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
  if args == []
    then printHelp
    else do let (command : t) = args
            dispatch command t

dispatch :: String -> [String] -> IO ()
dispatch k args = do
  let choice = Map.lookup k choices
  case choice of
    Nothing -> putStrLn "No command found"
    Just command -> command

printHelp :: IO ()
printHelp = do sequence_ $ Map.mapWithKey printCommand choices

printCommand :: String ->  IO () -> IO ()
printCommand s c = putStrLn s


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

readInput :: IO()
readInput =
  forever $ do
    line <- getLine
    putStrLn $ map toUpper line


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
