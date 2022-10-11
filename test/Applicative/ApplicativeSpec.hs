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

instance Functor' ((->)a) where
  fmap' f g = f . g


foo :: IO String
foo = do
  return "Hey"

spec :: Spec
spec = do
  it "fmap a function to IO" $ do
    r2 <- fmap' length foo
    r2 `shouldBe` 3

  it "fmap a function to funtions" $ do
    let f i = i + 3 :: Int
      in fmap' (*2) f 5 `shouldBe` 16
