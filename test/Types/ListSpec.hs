{-# OPTIONS_GHC -Wno-unused-matches #-}
module Types.ListSpec where

import Test.Hspec

-- Infix constructors must begin with a colon
infixr 9 :+
data List a  = Empty | a :+ (List a)

-- See Standalone Deriving Declarations
-- https://wiki.haskell.org/GHC/Stand-alone_deriving_declarations
deriving instance Show a => Show(List a)
deriving instance Eq a => Eq(List a)

len :: List a -> Int
len Empty = 0
len (a :+ l) = 1 + len l

concat' :: List a -> List a -> List a
concat' l Empty = l
concat' Empty r = r
concat' (h :+ t) r = h :+ concat' t r



spec :: Spec
spec = do
  it "creates a list" $ do
    let list1 = (1 :+ Empty) ::(List Int)
        list2 = (1 :+ 2 :+ Empty) ::(List Int)
        list3 = (1 :+ 2 :+ 3 :+ Empty) ::(List Int)
      in do len list1 `shouldBe` (1::Int)
            len list2 `shouldBe` (2::Int)
            len list3 `shouldBe` (3::Int)

  it "concatenates lists" $ do
    let list1 = (1 :+ Empty)
        list2 = (2 :+ 3 :+ Empty)
        expected = (1 :+ 2 :+ 3 :+ Empty)  :: (List Int)
          in concat' list1 list2 `shouldBe` expected
