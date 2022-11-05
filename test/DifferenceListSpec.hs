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

spec :: Spec
spec = do
  it "should append Difference Lists" $ do
    let l1  = diffList ([1,2,3]:: [Int])
        l2  = diffList ([10,20,30]:: [Int])
        l12 = l1 `append` l2 in
      getDiffList l12 [] `shouldBe` getDiffList (diffList ([1,2,3,10,20,30]:: [Int])) []
