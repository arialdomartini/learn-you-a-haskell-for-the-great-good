{-# LANGUAGE InstanceSigs #-}
module DifferenceListSpec where

import Test.Hspec

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

diffList :: [a] -> DiffList a
-- diffList eq xs = eq ++ xs
-- diffList eq = \xs -> eq ++ xs
diffList eq = DiffList (eq ++)

append :: DiffList a -> DiffList a -> DiffList a
-- f `append` g = \xs -> f (g xs)
append f g = DiffList (f' . g') where
  f' = getDiffList f
  g' = getDiffList g

fromDiffList :: DiffList a -> [a]
fromDiffList l = getDiffList l []

instance Semigroup (DiffList a) where
  (<>) :: DiffList a -> DiffList a -> DiffList a
  a <> b = a `append` b

instance Monoid (DiffList a) where
  mempty :: DiffList a
  -- mempty = diffList []
  mempty = DiffList id

  mappend = (<>)


spec :: Spec
spec = do
  it "should append Difference Lists" $ do
    let l1  = diffList ([1,2,3]:: [Int])
        l2  = diffList ([10,20,30]:: [Int])
        l12 = l1 `append` l2 in
      getDiffList l12 [] `shouldBe` getDiffList (diffList ([1,2,3,10,20,30]:: [Int])) []

  it "convert DiffList to ordinary List" $ do
    let l1  = diffList ([1,2,3]:: [Int])
        l2  = diffList ([10,20,30]:: [Int])
        l12 = l1 `append` l2 in
      fromDiffList l12 `shouldBe` [1,2,3,10,20,30]

  it "DiffList is an instance of Monoid" $ do
    let l1  = diffList ([1,2,3]:: [Int])
        l2  = diffList ([10,20,30]:: [Int]) in
      fromDiffList(l1 `mappend` l2) `shouldBe` [1,2,3,10,20,30]
