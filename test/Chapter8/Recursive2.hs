module Chapter8.Recursive2
  where
import Test.Hspec

main = hspec spec
spec = do
--  it "should build a list" $ do
--    toList (Cons 1 (Cons 2 (Cons 3 (Empty)))) `shouldBe` [1,2,3]

  it "should build a list with .>" $ do
    toList (1 .> 2 .> 3 .> Empty) `shouldBe` [1,2,3]

  it "should concatenate lists" $ do
    ( (1.>2.>Empty) .++ (3.>4.>Empty) ) `shouldBe` ( 1.>2.>3.>4.>Empty )


toList :: List a -> [a]
toList Empty = []
toList (a `Cons` rest) = a : (toList rest)

data List a = Empty | Cons a (List a) deriving (Eq, Show)

infixr 5 .>
(.>) :: a -> List a -> List a
(.>) a l = Cons a l

(.++) :: List a -> List a -> List a
Empty .++ bs = bs
(a `Cons` as) .++ bs = a.>(as .++ bs)
