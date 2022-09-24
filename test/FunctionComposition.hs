{-# OPTIONS_GHC -Wno-type-defaults #-}
module FunctionComposition where

import Test.Hspec


(/.) :: (b -> c) -> (a -> b) -> (a -> c)
(/.) f g n = f (g n)

spec :: Spec
spec = do
  it "composes functions" $ do
    (f /. g) 12 `shouldBe` f ( g 12 )
    where
      f = (* 2)
      g = (+ 12)
