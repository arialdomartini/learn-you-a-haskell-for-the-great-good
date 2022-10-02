{-# OPTIONS_GHC -Wno-unused-matches #-}
module Types.ListSpec where

import Test.Hspec

data List a  = Empty | a :+ (List a)


len :: List a -> Int
len Empty = 0
len (a :+ l) = 1 + len l

infixr 9 :+


spec :: Spec
spec = do
  it "creates a list" $ do
    let list1 = (1 :+ Empty) ::(List Int)
        list2 = (1 :+ 2 :+ Empty) ::(List Int)
        list3 = (1 :+ 2 :+ 3 :+ Empty) ::(List Int)
      in do len list1 `shouldBe` (1::Int)
            len list2 `shouldBe` (2::Int)
            len list3 `shouldBe` (3::Int)
