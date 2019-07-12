module Chapter7.SetSpec where
import Test.Hspec
import Data.Set as Set

main = hspec spec

spec :: Spec
spec = do
  it "should eliminate duplicates in set" $ do
    Set.fromList "abaaaacdeeee" `shouldBe` Set.fromList "abcde"
