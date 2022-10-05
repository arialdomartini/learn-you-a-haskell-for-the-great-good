module Main(main) where

main :: IO ()
main = do
   salute


salute :: IO ()
salute =
  do putStrLn "Hello World"
     putStrLn "Hello World. What's your name?"
     name <- getLine
     putStrLn $ "Hola, " ++ name
     putStrLn $ tellFortune name


tellFortune name = name ++ " you will be very lucky"
