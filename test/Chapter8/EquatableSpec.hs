module Chapter8.EquatableSpec where
import Test.Hspec

main = hspec spec
spec = do
  it "compares semaphore states" $ do
    Red ==. Green `shouldBe` False
    Red ==. Red `shouldBe` True

  it "should show semaphore states" $ do
    show Red `shouldBe` "Red light, stop"
    show Green `shouldBe` "Green light, go!"
    show Yellow `shouldBe` "Yellow light, wait..."

  it "can compare 2 Maybes" $ do
    Just 2 ==. Just 2 `shouldBe` True


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

instance Show Semaphore where
  show Red = "Red light, stop"
  show Green = "Green light, go!"
  show Yellow = "Yellow light, wait..."

instance (Eq a) => Eq' (Maybe a) where
  Nothing ==. Nothing = True
  Just a ==. Just b = a == b
  _ ==. _ = False
