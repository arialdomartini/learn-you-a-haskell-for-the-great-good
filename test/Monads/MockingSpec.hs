{-# LANGUAGE GADTs #-}
module Monads.MockingSpec where

import Test.Hspec
import Data.Char (toUpper)

class DbMonad a where
  save :: String -> a

data MockDb where
  MockDb :: String -> MockDb
  deriving (Eq, Show)

data RealDb where
  RealDb :: IO () -> RealDb

instance DbMonad MockDb where
  save s = MockDb ("Saved " ++ s)

instance DbMonad RealDb where
  save s = RealDb $ putStrLn "No db available!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"


updateUser :: DbMonad a => String -> a
updateUser userName = do
  let upper = fmap toUpper userName
  save upper


mockedUpdateUser :: String -> MockDb
mockedUpdateUser = updateUser

spec :: Spec
spec = do
  it "should mock" $ do
    mockedUpdateUser "Joe" `shouldBe` MockDb "Saved JOE"
    (updateUser "Joe" :: MockDb) `shouldBe` MockDb "Saved JOE"

  it "should run in production" $ do
    let (RealDb a) = (updateUser "Joe" :: RealDb)
    x <- a
    x `shouldBe` ()
