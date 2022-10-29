{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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


newtype MockFS a = MockFS (Identity a) deriving (Functor, Applicative, Monad, Show, Eq)

instance MonadFS MockFS where
  myReadFile filePath = MockFS (return $ "content of file " <> filePath)
  myWriteFile filePath content = MockFS $ return ()

spec :: Spec
spec = do
  it "should mock" $ do
    (withSideEffects "foo.txt" "new content" :: MockFS String) `shouldBe` MockFS (return "content of file foo.txt+new content")
