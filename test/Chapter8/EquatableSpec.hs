module Chapter8.EquatableSpec where
import Test.Hspec

main = hspec spec
spec = do
  it "compares semaphore states" $ do
    Red ==. Green `shouldBe` False
    Red ==. Red `shouldBe` True

data Semaphore = Red | Green | Yellow

class Eq' a where
  (==.) :: a -> a -> Bool
  (/=.) :: a -> a -> Bool
  a ==. b = not (a /=. b)
  a /=. b = not (a ==. b)

instance Eq' Semaphore where
  Red ==. Red = True
  Green ==. Green = True
  Yellow ==. Yellow = True
  _ ==. _ = False
