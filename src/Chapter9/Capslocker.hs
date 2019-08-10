module Chapter9.Capslocker where
import Data.Char

main = do
  content <- getContents
  putStrLn (map toUpper content)
