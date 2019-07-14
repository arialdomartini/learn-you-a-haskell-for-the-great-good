module Chapter8.YesNoSpec where
import Test.Hspec

main = hspec spec
spec = do

  it "empty List is False" $ do
    yesno [] `shouldBe` False

  it "not empty List is True" $ do
    yesno [1,2,3] `shouldBe` True

class YesNo a where
  yesno :: a -> Bool

instance YesNo [a] where
  yesno [] = False
  yesno _ = True
