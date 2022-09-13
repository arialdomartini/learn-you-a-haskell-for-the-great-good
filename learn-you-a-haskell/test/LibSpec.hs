module LibSpec(spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do

  it "doubles numbers" $ do
    (20::Int) `shouldBe` (double 10)

  it "concatenates lists" $ do
    [1,2,3] ++ [4,5,6] `shouldBe` ([1,2,3,4,5,6]::[Int])

  it "cons elements to lists" $ do
    'a' : ['b', 'c'] `shouldBe` ['a', 'b', 'c']

  it "cons elements to infite lists, as it has not to walk through them" $ do
    let numbers = [1..] :: [Int]
    let with0 = 0 : numbers
    take 10 with0 `shouldBe` [0,1,2,3,4,5,6,7,8,9]


  it "concatenating elements to infite lists; it works because Haskell is lazy" $ do
    let numbers = [1..] :: [Int]
    let concatenated = numbers ++ [100,99]

    take 10 concatenated `shouldBe` [1,2,3,4,5,6,7,8,9,10]
