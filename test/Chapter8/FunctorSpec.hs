module Chapter8.FunctorSpec where
import Test.Hspec

main = hspec spec
spec = do
  it "List should be a functor" $ do
    fmap' (*2) [1,2,3] `shouldBe` [2,4,6]

  it "Maybe should be a functor" $ do
    fmap' (+10) (Just 8) `shouldBe` Just 18
    fmap' (+10) Nothing `shouldBe` Nothing

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' g f = map g f

instance Functor' Maybe where
  fmap' _ Nothing = Nothing
  fmap' g (Just x) = Just (g x)
