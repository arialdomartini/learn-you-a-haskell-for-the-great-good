{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Monads.MockingMonadSpec where

import Test.Hspec
import Data.Functor.Identity (Identity)


saveIO :: String -> IO ()
saveIO s = putStrLn $ "Saved " <> s

withSideEffects :: MonadFS m => FilePath -> String -> m String
withSideEffects fileName newContent = do
  content <- myReadFile fileName
  let tot = content <> "+" <> newContent
  myWriteFile fileName tot
  return tot

class Monad m => MonadFS m where
  myReadFile :: FilePath -> m String
  myWriteFile :: FilePath -> String -> m ()


instance MonadFS IO where
  myReadFile = readFile
  myWriteFile = writeFile


class Functor a => Result a where
  length' :: String -> a Int

data Mock a = Mock a deriving (Functor, Show, Eq)
data Prod a = Prod a deriving (Functor, Show, Eq)

instance Result Mock where
  length' s = Mock 100

instance Result Prod where
  length' s = Prod (length s)

elevated :: Int -> Int
elevated = (^(8::Int))

-- calculate :: Result a => Int -> a String
calculate :: (Result f) => Int -> f String
calculate n =
  let r = length' (show  (elevated n)) in
    fmap show r

newtype MockFS a = MockFS (Identity a) deriving (Functor, Applicative, Monad, Show, Eq)

instance MonadFS MockFS where
  myReadFile filePath = MockFS (return $ "content of file " <> filePath)
  myWriteFile filePath content = MockFS $ return ()

spec :: Spec
spec = do
  it "should mock" $ do
    (withSideEffects "foo.txt" "new content" :: MockFS String) `shouldBe` MockFS (return "content of file foo.txt+new content")

  it "should mock non IO operations" $ do
    (calculate 2 :: Prod String)`shouldBe` (Prod "3")

  it "should mock non IO operations" $ do
    (calculate 2 :: Mock String)`shouldBe` (Mock "100")
