module Monads.SafeRPNSpec where

import Test.Hspec
import Control.Monad (foldM)

type Value = Double
type Stack = [Value]

calc :: String -> Value
calc = head . foldl folding [] . words

folding :: Stack -> String -> Stack
folding (a:b:xs) "+"  = (b + a) : xs
folding (a:b:xs) "*" = (b * a) : xs
folding xs n = (read n:: Value) : xs

-- foldM (b -> a -> m b) b (t a)

--foldM' :: (b -> a -> m b) -> b -> t a -> m b

calcSafe :: String -> Maybe Value
calcSafe = head . sequence . foldM folding' [] . words

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

folding' :: Stack -> String -> Maybe Stack
folding' (a:b:xs) "+"  = Just ((b + a) : xs)
folding' (a:b:xs) "*" = Just ((b * a) : xs)
folding' xs n = fmap (:xs) (readMaybe n)




spec :: Spec
spec = do
  it "RPN calculator" $ do
    calc "4" `shouldBe` 4
    calc "4 2 +" `shouldBe` 6
    calc "4 2 + 3 3 + *" `shouldBe` (4 + 2) * (3 + 3)

  it "safe RPN calculator" $ do
    calcSafe "4 2 + 3 3 + *" `shouldBe` Just ((4 + 2) * (3 + 3))
    calcSafe "+ 3 3 + *" `shouldBe` Nothing
