{-# LANGUAGE FlexibleInstances #-}
module Types.YesNoSpec where

import Test.Hspec

class YesNo a where
  if' :: a -> Bool

instance YesNo Int where
  if' i = i /= 0

-- This requires FlexibleInstances!
instance YesNo String where
  if' s = s /= ""

instance YesNo Bool where
  if' = id

instance YesNo (Maybe a) where
  if' Nothing = False
  if' (Just _) = True

spec :: Spec
spec = do
  it "converts integers to Bool" $ do
    if' (2::Int) `shouldBe` True
    if' (0::Int) `shouldBe` False

  it "converts strings to Bool" $ do
    if' "joe" `shouldBe` True
    if' "" `shouldBe` False

  it "works with ordinary Bool values" $ do
    if' True `shouldBe` True
    if' False `shouldBe` False

  it "works with Maybe" $ do
    if' Nothing `shouldBe` False
    if' (Just "hey") `shouldBe` True
