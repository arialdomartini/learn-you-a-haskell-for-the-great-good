module Applicative.ApplicativeSpec where

import Test.Hspec

class Functor' m where
  fmap' :: (a -> b) -> m a -> m b

instance Functor' IO where
  -- Linter suggests to replace this with
  -- f <$> m
  fmap' f m = do
    res <- m
    return $ f res

foo :: IO String
foo = do
  return "Hey"

spec :: Spec
spec = do
  it "fmap a function to IO" $ do
    r2 <- fmap' length foo
    r2 `shouldBe` 3
