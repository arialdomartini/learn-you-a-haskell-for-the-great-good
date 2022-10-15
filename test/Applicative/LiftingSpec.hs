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
    (sequence [(+3), (^2), (+1000)] 10) `shouldBe` [13, 100,1010]

  it "combines functions" $ do
    let r = (*) <$> (+2) <*> (+3)
     in r 100 `shouldBe` ((100 + 2) * (100 + 3) :: Int)

  it "implements length with const and (+1)" $ do
    let length' = foldl (const . (+1)) 0
        xs = [1,2,3,4,5,6,7,8,9] :: [Int] in
      length' xs `shouldBe` length xs

  it "sequencing lists of lists" $ do
    sequenceA [[1],[2]] `shouldBe` ([[1,2]] :: [[Int]])
    sequenceA [[1,2], [3,4]] `shouldBe` ([[1,3], [1,4], [2,3], [2, 4]] :: [[Int]]) -- Not easy to grasp!

  it "uses sequenceA for checking if all predicates are satisfied" $ do
      let predicates = [(>0), (>10), odd, even . (*2)] in
        all (==True) (sequence predicates (11::Int)) `shouldBe` True

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (h:t) = (:) <$> h <*> sequenceA' t
                -- liftA2 (:) h (sequenceA' t)
                -- h <*> (pure (:)) <*> sequenceA' t
                -- foldr (liftA2 (:)) (pure [])
