module Lib
    ( someFunc, double
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

double :: Num a => a -> a
double x = x + x
