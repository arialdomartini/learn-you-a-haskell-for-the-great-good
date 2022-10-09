module RPNSpec where

import Test.Hspec

type Token = String
type Tokens = [Token]
type RawInput = String
newtype Stack = Stack Tokens
type Operation = Double -> Double -> Double

solve :: RawInput -> Double
solve = evaluate . tokenize

tokenize :: RawInput -> Tokens
tokenize = words

empty :: Stack
empty = Stack []

-- Pop
pop :: Stack -> (Token, Stack)
pop (Stack [v]) = (v, empty)
pop (Stack (h : t))  = (h, Stack t)

-- Push
(>>>) :: Token -> Stack -> Stack
(>>>) v (Stack vs) = Stack (v : vs)


-- (b -> a -> b) -> b -> t a -> b
consume :: Stack -> Token -> Stack
consume stack "+" = doSum stack
consume stack "*" = doProduct stack
consume stack h   = h >>> stack

-- is this an implementation of a Lisp?
evaluate :: Tokens -> Double
evaluate tokens =
  let (Stack v) = foldl consume (Stack []) tokens
    in (read . head) v

-- adding new operations is just as simple as this.
doSum :: Stack -> Stack
doSum = doOperation (+)

doProduct :: Stack -> Stack
doProduct = doOperation (*)

doOperation :: Operation -> Stack -> Stack
doOperation operation (Stack s) =
  let (v1:v2:t) = s
      result = read v1 `operation` read v2
      in Stack ((show result) : t)


spec :: Spec
spec = do
  it "solve a value" $ do
    solve "42" `shouldBe` 42

  it "splits the input into words" $ do
    tokenize "42 34 + 100 2 / *" `shouldBe` ["42", "34", "+", "100", "2", "/", "*"]

  it "solves a sum" $ do
    solve "2 3 +" `shouldBe` 5

  it "solves a series of sums" $ do
    solve "2 3 + 3 3 + +" `shouldBe` 11

  it "solves a series of sums and products" $ do
    solve "2 3 * 3 3 * +" `shouldBe` 15
