module Main(main) where

main :: IO ()
main = do
   putStrLn "Hello World"
   putStrLn "Hello World. What's your name?"
   name <- getLine
   putStrLn $ "Hola, " ++ name
