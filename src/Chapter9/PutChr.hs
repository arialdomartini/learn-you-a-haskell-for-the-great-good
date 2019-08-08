module Chapter9.PutChr where

main :: IO ()
main = do
  putStr' "Hello, world"

putStr' :: String -> IO ()
putStr' s = case s of
  "" -> do
    return ()
  (x:xs) -> do
    putChar x
    putStr' xs
