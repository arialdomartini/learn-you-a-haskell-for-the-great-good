module Chapter9.ShorterLines where

main = do
  content <- getContents
  putStrLn $ onlyShorter content

onlyShorter :: String -> String
onlyShorter content = unlines filteredLines where
  filteredLines = filter isLongerThan10 allLines
  isLongerThan10 l = length l > 10
  allLines = lines content
