module Monads.SafeRPNSpec where

import Test.Hspec

type Value = Double
type Stack = [Value]

calc :: String -> Value
calc = head . foldl folding [] . words

folding :: Stack -> String -> Stack
folding (a:b:xs) "+"  = (b + a) : xs
folding (a:b:xs) "*" = (b * a) : xs
folding xs n = (read n:: Value) : xs


spec :: Spec
spec = do
  it "RPN calculator" $ do
    calc "4" `shouldBe` 4
    calc "4 2 +" `shouldBe` 6
    calc "4 2 + 3 3 + *" `shouldBe` (4 + 2) * (3 + 3)
