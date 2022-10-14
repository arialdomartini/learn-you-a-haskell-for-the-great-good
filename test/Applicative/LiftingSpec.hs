module Applicative.LiftingSpec where

import Test.Hspec
import Control.Applicative

f :: String -> String
f = (++ "!")

f2 :: String -> Int -> String
f2 s i = s ++ "-" ++ show i

spec :: Spec
spec = do

  -- (<*>) :: Applicative f  =>  f (a -> b)       -> f a -> f b
  -- liftA :: Applicative f  =>    (a -> b)       -> f a -> f b
  it "liftA and its equivalences" $ do
    let r1 = pure f <*>  Just "Hey Joe"
        r2 = liftA f $   Just "Hey Joe"
        r3 = f       <$> Just "Hey Joe"
        r4 = (fmap f) $  Just "Hey Joe" in do
      r1 `shouldBe` r2
      r2 `shouldBe` r3
      r3 `shouldBe` r4

  -- liftA2 :: Applicative f =>    (a -> b -> c)  -> f a -> f b -> f c
  it "liftA2 and its equivalences" $ do
    let r1 = pure f2 <*> Just "Hey" <*> Just 42
        r2 = fmap f2 (Just "Hey") <*> Just 42
        r3 = f2 <$> (Just "Hey") <*> Just 42
        r4 = liftA2 f2 (Just "Hey") (Just 42)
      in do r1 `shouldBe` r2
            r2 `shouldBe` r3
            r3 `shouldBe` r4

  it "combines 2 applicatives" $ do
    let a = Just 3 :: Maybe Int
        b = fmap (: []) (Just 4) -- Just [4]
      in liftA2 (:) a b `shouldBe` Just [3,4]
