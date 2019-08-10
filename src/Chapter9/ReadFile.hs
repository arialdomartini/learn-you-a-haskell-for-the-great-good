module Chapter9.ReadFile where

import System.IO

main = do
  -- using openFile
  handle <- openFile "src/Chapter9/ReadFile.hs" ReadMode
  content <- hGetContents handle
  putStrLn content
  hClose handle

  putStrLn "-- using withFile"
  withFile "src/Chapter9/ReadFile.hs"
    ReadMode (\handle -> do
                 content <- hGetContents handle
                 putStrLn content)

  putStrLn "-- using a custom implementation of withFile"
  withFile' "src/Chapter9/ReadFile.hs"
    ReadMode (\handle -> do
                 content <- hGetContents handle
                 putStrLn content)

  putStrLn "-- using readFile"
  content <- readFile "src/Chapter9/ReadFile.hs"
  putStrLn content

  putStrLn "-- using a custom implementation of readFile  (this gives an error in GHCI, and also when compiled)"
  content <- readFile' "src/Chapter9/ReadFile.hs"
  putStrLn content

withFile' :: FilePath -> IOMode -> (Handle -> IO ()) -> IO ()
withFile' filePath fileMode f = do
  handle <- openFile filePath fileMode
  result <- f handle
  hClose handle
  return result

readFile' :: FilePath -> IO String
readFile' filePath = do
  handle <- openFile filePath ReadMode
  content <- hGetContents handle
--  hFlush handle
  hClose handle
  return content
