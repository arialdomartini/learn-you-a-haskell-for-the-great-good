module Chapter8.RecursiveSpec
  where
import Test.Hspec

main = hspec spec
spec = do
--  it "should build a list" $ do
--    toList (Cons 1 (Cons 2 (Cons 3 (Empty)))) `shouldBe` [1,2,3]

  it "should build a list with :>" $ do
    toList (1 :> 2 :> 3 :> Empty) `shouldBe` [1,2,3]

  it "should concatenate lists" $ do
    ( (1:>2:>Empty) .++ (3:>4:>Empty) ) `shouldBe` ( 1:>2:>3:>4:>Empty )

infixr 5 :>
data List a = Empty | a :> (List a) deriving (Eq, Ord, Show)
toList :: List a -> [a]
toList Empty = []
toList (a :> rest) = a : (toList rest)

-- alternatively
-- data List a = Empty | Cons a (List a)
-- (:>) :: a -> List a -> List a
-- (:>) a l = Cons a l
-- but this wouldn't have worked with pattern matching!

(.++) :: List a -> List a -> List a
Empty .++ bs = bs
(a:>as) .++ bs = a:>(as .++ bs)
