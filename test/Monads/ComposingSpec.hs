module Monads.ComposingSpec where

import Test.Hspec
import Control.Monad ((<=<), foldM)

(<=<<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=<< g = \a ->
  let r = g a in
    r >>= f

g' :: String -> Maybe Int
g' s = Just (length s)

f' :: Int -> Maybe Int
f' n = Just (n * 2)

spec :: Spec
spec = do
  it "combine monadic functions" $ do
    (f' <=<< g') "hey!" `shouldBe` (f' <=< g') "hey!"
