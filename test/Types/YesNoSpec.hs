{-# LANGUAGE FlexibleInstances #-}
module Types.YesNoSpec where

import Test.Hspec

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno i = i /= 0

-- This requires FlexibleInstances!
instance YesNo String where
  yesno s = s /= ""

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True

if' :: YesNo a => a -> b -> b -> b
if' yn t f = if (yesno yn) then t else f

spec :: Spec
spec = do
  it "converts integers to Bool" $ do
    yesno (2::Int) `shouldBe` True
    yesno (0::Int) `shouldBe` False

  it "converts strings to Bool" $ do
    yesno "joe" `shouldBe` True
    yesno "" `shouldBe` False

  it "works with ordinary Bool values" $ do
    yesno True `shouldBe` True
    yesno False `shouldBe` False

  it "works with Maybe" $ do
    yesno Nothing `shouldBe` False
    yesno (Just "hey") `shouldBe` True

  it "works an an ordinary if then else" $ do
    if' Nothing "yes" "no" `shouldBe` "no"
    if' (Just 4:: Maybe Int) "yes" "no" `shouldBe` "yes"
