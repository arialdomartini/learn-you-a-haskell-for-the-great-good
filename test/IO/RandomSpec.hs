{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE UndecidableInstances #-}
module IO.RandomSpec where

import Test.Hspec
import System.Random (StdGen, Random (random))
import System.Random.Stateful (mkStdGen)

data MyRand v = MyRand (StdGen -> (v, StdGen))


instance Functor MyRand where
  fmap f (MyRand g) =
    MyRand (\s ->
              let (v, s') = g s
                  v' = f v in
                    (v', s'))


instance Functor MyRand => Applicative MyRand where
  pure :: a -> MyRand a
  pure v = MyRand (\s -> (v ,s))
  (<*>) :: MyRand (a -> b) -> MyRand a -> MyRand b
  (<*>) (MyRand r') (MyRand r'') =
         MyRand (\s ->
                   let (f', _) = r' s           -- f' = a -> b
                       (v'', s'') = r'' s  -- s -> (a, s')
                       v''' = f' v'' in
                         (v''', s''))

instance Functor MyRand => Monad MyRand where
  (>>=) :: MyRand a -> (a -> MyRand b) -> MyRand b
  (>>=) (MyRand m) f =
    MyRand (\s ->
      let (v, s') = m s
          MyRand f' = f v
          (v'', s'') = f' s' in
        (v'', s''))

run :: MyRand a -> StdGen -> a
run (MyRand f) gen =
  let (v, _) = f gen
  in v

randoms' :: StdGen -> [Int]
randoms' gen =
  r : rest where
    (r, gen') = random gen :: (Int, StdGen)
    rest = randoms' gen'


spec :: Spec
spec = do
  it "generates pseudo random numbers" $ do
    let (r1, g1) = random (mkStdGen 100) :: (Int, StdGen)
        (r2, _) = random g1 :: (Int, StdGen)
      in do r1 `shouldBe` 9216477508314497915
            r2 `shouldBe` (-6917749724426303066)

  it "generates a pseudo random number using Monad" $ do
    let gen = mkStdGen 100 :: StdGen
    let r = MyRand random :: (MyRand Int)
    run r gen `shouldBe` 9216477508314497915

  it "generates more pseudo random numbers using Monad" $ do
    let gen = mkStdGen 100 :: StdGen
    let myRand = MyRand random :: (MyRand Int)
    let r'' = myRand >>= (\v' -> MyRand (\s ->
                                           let (v'', s'') = random s
                                             in ((v', v''), s''))) :: (MyRand (Int, Int))
    let v = run r'' gen in
          v `shouldBe` (9216477508314497915,-6917749724426303066)

  it "generates an infinite list of random number with a recursive function" $ do
    take 4 (randoms' (mkStdGen 100)) `shouldBe` [9216477508314497915, -6917749724426303066, -2348976503111297336, -716157807093485800]
