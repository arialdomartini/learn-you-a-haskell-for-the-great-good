module Chapter9.ConvertToCaps where
import Data.Char

main = do
  content <- readFile inFilePath
  writeFile outFilePath $ convertToCaps content where

    inFilePath = "src/Chapter9/ConvertToCaps.hs"
    outFilePath = toOut inFilePath
    toOut s = s ++ ".caps"
    
    convertToCaps :: String -> String
    -- convertToCaps s = unlines (map toUpperString (lines s))
    convertToCaps = unlines . (map toUpperString)  . lines
    
    toUpperString = map toUpper
