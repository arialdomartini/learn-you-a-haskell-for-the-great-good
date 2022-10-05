{-# LANGUAGE GADTs #-}
module IO.IOSpec where

import Test.Hspec
import Data.Char (toUpper)

-- A function returning IO
f :: IO String
f = do return "mario"

printHelloProduction :: IO()
printHelloProduction = printHello putStrLn

printHello :: (String -> IO ()) -> IO ()
printHello pr =  pr "Hello, side-effectful world!"


saluteProd :: IO ()
saluteProd = salute (Input {put = putStrLn, get= getLine})


data Input where
  Input :: {put :: String -> IO (), get :: IO String} -> Input

salute :: Input -> IO ()
salute input = do
  put input "What's your first name?"
  firstName <- get input
  put input "What's your last name?"
  lastName <- get input
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  put input $ "hey " ++ bigFirstName ++ " "
    ++ bigLastName
    ++ ", how are you?"

spec :: Spec
spec = do

  it "stubs IO using a Data Type" $ do
    let putStub s = return ()
    let getStub = return "Mario"
    let saluteTest = salute (Input {put = putStub, get= getStub})
    result <- saluteTest
    result `shouldBe` ()


  -- asserting an IO
  it "cracks open an IO" $ do
    result <- f
    result `shouldBe` "mario"

  -- this replaces putStrLn with a stub
  it "mocks putStrLn" $ do
    let pr s = do
          s `shouldBe` "Hello, side-effectful world!"
          return () -- putStrLn is mocked
    result <- printHello pr
    result `shouldBe` ()

  -- this executes the side effect
  it "test putStrLn" $ do
    printHello putStrLn `shouldReturn` ()
