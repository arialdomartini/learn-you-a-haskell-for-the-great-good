module Monads.FailuresSpec where

import Test.Hspec
import Control.Exception

data Foo a = Foo a deriving (Show, Eq)

instance Functor Foo where
  fmap f (Foo a) = Foo (f a)

instance Applicative Foo where
  Foo f <*> Foo v = Foo (f v)
  pure x = Foo x

instance Monad Foo where
  (Foo a) >>= f = f a

instance MonadFail Foo where
  fail _ = error "This is a custom error message"

myHead :: String -> Foo Char
myHead x = do
    (x:xs) <- Foo x
    return x


spec :: Spec
spec = do
  it "should use the monadic error" $ do
    evaluate((myHead "")) `shouldThrow` (errorCall "This is a custom error message")
