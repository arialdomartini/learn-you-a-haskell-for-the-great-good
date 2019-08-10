module Chapter9.ReadFile where

import System.IO

main = do
  -- using openFile
  handle <- openFile "src/Chapter9/ReadFile.hs" ReadMode
  content <- hGetContents handle
  putStrLn content
  hClose handle

  -- using withFile
  withFile "src/Chapter9/ReadFile.hs"
    ReadMode (\handle -> do
                 content <- hGetContents handle
                 putStrLn content)

  -- using a custom implementation of withFile
  withFile' "src/Chapter9/ReadFile.hs"
    ReadMode (\handle -> do
                 content <- hGetContents handle
                 putStrLn content)

withFile' :: FilePath -> IOMode -> (Handle -> IO ()) -> IO ()
withFile' filePath fileMode f = do
  handle <- openFile filePath fileMode
  result <- f handle
  hClose handle
  return result
