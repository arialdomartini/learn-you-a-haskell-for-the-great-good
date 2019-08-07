module Chapter9.HelloWorld where

-- run it with Chapter9.HelloWorld.main
main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hi, " ++ name)
