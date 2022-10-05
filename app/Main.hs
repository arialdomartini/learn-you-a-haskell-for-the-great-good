module Main(main) where

main :: IO ()
main = do
  forEverPrint
  salute



forEverPrint :: IO ()
forEverPrint =
  do
    line <- getLine
    putStrLn $ reverse line
    forEverPrint


salute :: IO ()
salute =
  do putStrLn "Hello World"
     putStrLn "Hello World. What's your name?"
     name <- getLine
     putStrLn $ "Hola, " ++ name
     putStrLn $ tellFortune name


tellFortune name = name ++ " you will be very lucky"
