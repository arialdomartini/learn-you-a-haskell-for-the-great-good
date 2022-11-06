module Monads.ReaderSpec where

import Test.Hspec
import Control.Monad.Reader

class Monad' m where
  return' ::  a -> m a
  (>>>=) :: m a -> (a -> m b) -> m b

instance Monad' ((->) a) where
  return' a = \_ -> a
  -- f1 >>>= g2 = \a -> g1 (f1 a) a
  f1 >>>= g1 = \a ->
    let r = f1 a
        r' = g1 r in
      r' a


f' :: String -> (String -> Int)
f' s = \x -> length (s ++ x)

g' :: Int -> (String -> Int)
g' n = \x -> (length x) + (n *2)



f :: String -> (String -> Int)
f s = \x -> length (s ++ x)

g :: Int -> (String -> Int)
g n = \x -> (length x) + (n *2)

bound :: String -> Int
bound = do
  r  <- f "hey"
  r' <- g r
  return r'

op :: Int -> Int
op = do
  a <- (*2)     -- 10 * 2 = 20
  b <- (+1)     -- 10 + 1 = 11
  return (a + b)  -- 20 + 11 = 31

opr :: Reader Int Int
opr = do
  i <- ask
  let a = i * 2
  let b = i +1
  return (a + b)


spec :: Spec
spec = do
  it "binds 2 functions" $ do
    ((f "hey") >>= g)("joe") `shouldBe` 15

  it "binds 2 functions using the do notation" $ do
    bound "joe" `shouldBe` 15

  it "binds 2 functions using the custom implementation" $ do
    ((f' "hey") >>>= g')("joe") `shouldBe` 15

  it "uses functions as monads" $ do
    op 10 `shouldBe` 31

  it "uses functions as monads, using Reader" $ do
    (runReader opr) 10 `shouldBe` 31
