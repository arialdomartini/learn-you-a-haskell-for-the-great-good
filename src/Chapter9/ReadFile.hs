module Chapter9.ReadFile where

import System.IO

main = do
  handle <- openFile "src/Chapter9/ReadFile.hs" ReadMode
  content <- hGetContents handle
  putStrLn content
  hClose handle
