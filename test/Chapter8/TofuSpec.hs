module Chapter8.TofuSpec where

import Test.Hspec

spec = do
  it "should fail" $ do
    Foo { field = Just "hello" } `shouldBe` Foo { field = Just "hello" }
    Foo { field = Just 'x' } `shouldBe` Foo { field = Just 'x' }
    (tofu (Just 'x') :: (Foo Char Maybe)) `shouldBe` Foo { field = Just 'x' }


data Foo v c = Foo { field :: c v } deriving (Show, Eq)

class Tofu t where
-- v :: *                       Int
-- c :: * -> *                  Option v
-- t :: * -> (* -> *) -> *      Int -> Option v -> Foo Int Option
  tofu :: v c -> t c v

instance Tofu Foo where
  tofu c = Foo c
