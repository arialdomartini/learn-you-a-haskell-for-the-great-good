module Main(main) where

main :: IO ()
main = do
  sequence forEverPrint
  salute



forEverPrint :: [IO ()]
forEverPrint = repeat singlePrint

singlePrint :: IO ()
singlePrint = do
    line <- getLine
    putStrLn $ reverse line


salute :: IO ()
salute =
  do putStrLn "Hello World"
     putStrLn "Hello World. What's your name?"
     name <- getLine
     putStrLn $ "Hola, " ++ name
     putStrLn $ tellFortune name


tellFortune name = name ++ " you will be very lucky"
