{-# LANGUAGE GADTs #-}
module IO.IOSpec where

import Test.Hspec
import Data.Char (toUpper)
import GHC.IORef (IORef(IORef), newIORef, writeIORef)
import Data.IORef (readIORef)

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

putStub :: IORef String -> String -> IO ()
putStub ref s = do
  writeIORef ref s
  return ()
getStub s = return s

createForTest :: IORef String -> IO Input
createForTest ioR = do
  v <- readIORef ioR
  return (Input {put = putStub ioR, get= getStub v})

ioR :: IO (IORef String)
ioR = newIORef "Mario"

someAction :: IO (String)
someAction = do return "Hello"

sequence' :: [ IO a ] -> IO [a]
sequence' [] = do return []
sequence' (x:xs) =
  do
    r <- x
    rest <- sequence' xs
    return (r : rest)


spec :: Spec
spec = do

  it "stubs IO using a Data Type" $ do
    ioRv <- ioR
    inputStub <- createForTest ioRv
    let saluteTest = salute inputStub
    saluteTest
    v <- readIORef ioRv
    v `shouldBe` "hey MARIO MARIO, how are you?"


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

  it "implements sequence t (m a) -> m (t a)" $ do
    let action1 = someAction
    let action2 = someAction
    let action3 = someAction
    let tma = [action1, action2, action3]
    s <- sequence' tma
    s `shouldBe` ["Hello", "Hello", "Hello"]
