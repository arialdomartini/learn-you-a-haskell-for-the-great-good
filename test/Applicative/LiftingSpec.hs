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
      in do liftA2 (:) a b `shouldBe` Just [3,4]
            (:) <$> a <*> b `shouldBe` Just [3,4]

  -- Let’s try implementing a function that takes a list of applicative values
  -- and returns an applicative value that has a list as its result value. We’ll call it
  -- sequenceA
  it "sequences applicatives" $ do
    (sequenceA' [ Just 3, Just 4, Just 5 ]) `shouldBe` Just [3,4,5]


  it "sequences functions" $ do
    (sequence [(+3), (^2), (+1000)] 10) `shouldBe` [30, 100,1010]


sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (h:t) = (:) <$> h <*> sequenceA' t
                -- liftA2 (:) h (sequenceA' t)
                -- h <*> (pure (:)) <*> sequenceA' t
                -- foldr (liftA2 (:)) (pure [])
