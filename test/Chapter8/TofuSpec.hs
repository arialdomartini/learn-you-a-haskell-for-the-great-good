module Chapter8.TofuSpec where

import Test.Hspec

spec = do
  it "should fail" $ do
    Foo (Just "hello") `shouldBe` Foo (Just "hello" )
    Foo (Just 'x') `shouldBe` Foo (Just 'x' )
    (tofu (Just 'x') :: (Foo Char Maybe)) `shouldBe` Foo (Just 'x')


data Foo v c = Foo (c v) deriving (Show, Eq)

class Tofu t where
-- v :: *                       Int
-- c :: * -> *                  Option v
-- t :: * -> (* -> *) -> *      Int -> Option v -> Foo Int Option
  tofu :: v c -> t c v

instance Tofu Foo where
  tofu c = Foo c


getFrank :: Frank Int Maybe
getFrank = Frank { frankField = Just 5 }

-- Frank is of kind * -> (* -> *) -> *
data Frank a b = Frank {frankField :: b a} deriving Show

instance Tofu Frank where
  tofu c = Frank {frankField = c}
