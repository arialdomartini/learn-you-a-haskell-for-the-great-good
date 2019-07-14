module Chapter8.FunctorSpec where
import Test.Hspec

main = hspec spec
spec = do
  it "List should be a functor" $ do
    fmap' (*2) [1,2,3] `shouldBe` [2,4,6]

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' g f = map g f
