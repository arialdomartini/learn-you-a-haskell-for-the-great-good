module Chapter8.RecursiveSpec
  where
import Test.Hspec

main = hspec spec
spec = do
  it "should pass" $ do
    toList (Cons 1 (Cons 2 (Cons 3 (Empty)))) `shouldBe` [1,2,3]


data List a = Empty | Cons a (List a)
toList :: List a -> [a]
toList Empty = []
toList (Cons a rest) = a : (toList rest)
