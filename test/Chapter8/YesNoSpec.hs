module Chapter8.YesNoSpec where
import Test.Hspec

main = hspec spec
spec = do

  it "empty List is False" $ do
    yesno [] `shouldBe` False

  it "not empty List is True" $ do
    yesno [1,2,3] `shouldBe` True

  it "Maybe as instance of YesNo" $ do
    yesno (Just 2) `shouldBe` True
    yesno Nothing `shouldBe` False
    yesno (Just False) `shouldBe` True
  
class YesNo a where
  yesno :: a -> Bool

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True
