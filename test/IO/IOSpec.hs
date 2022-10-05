module IO.IOSpec where

import Test.Hspec

-- A function returning IO
f :: IO String
f = do return "mario"

printHelloProduction :: IO()
printHelloProduction = printHello putStrLn

printHello :: (String -> IO ()) -> IO ()
printHello pr =  pr "Hello, side-effectful world!"

spec :: Spec
spec = do
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
