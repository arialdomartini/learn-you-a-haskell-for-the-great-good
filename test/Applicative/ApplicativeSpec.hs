{-# OPTIONS_GHC -Wno-type-defaults #-}
module Applicative.ApplicativeSpec where

import Test.Hspec

class Functor f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe where
  pure' = Just
  Nothing <**> _ = Nothing
  -- more concisely:
  -- Just f <**> v = fmap f v
  Just f <**> Just v = Just (f v)
  Just _ <**> Nothing = Nothing

instance Applicative' [] where
  pure' a = [a]
  (<**>) fs vs = [f v | f <- fs, v <- vs]

instance Applicative' IO where
  pure' a = do return a
  (<**>) fio vio = do
    f <- fio
    v <- vio
    return (f v)
    -- otherwise
    --  (<**>) fio vio = do
    --   f <- fio
    --   fmap f vio

instance Applicative' ((->)a) where
  pure' = const
  (<**>) f v = do
    f' <- f
    fmap f' v

data ZipList' a = ZipList' [a] deriving (Eq, Show)


instance Functor ZipList' where
  fmap _ (ZipList' []) = ZipList' []
  fmap f (ZipList' (h:t)) =
    let v1 = f h
        (ZipList' rest) = fmap f (ZipList' t) in
      ZipList' (v1 : rest)

instance Applicative' ZipList' where
  pure' v = ZipList' [v]
  -- (ZipList' []) <**> (ZipList' []) = ZipList' []
  -- (ZipList' (f:fs)) <**> (ZipList' (v:vs)) =
  --   let h = f v
  --       (ZipList' rest) = ZipList' fs <**> ZipList' vs -- ZipList rest
  --     in ZipList' (h : rest)
  (ZipList' fs) <**> (ZipList' vs) = ZipList' $ zipWith (\f v -> f v) fs vs

spec :: Spec
spec = do
  it "implements applicative for Maybe" $ do
    let m = Just 10 :: (Maybe Int)
        f = (*)
        a = fmap f m -- Just (10*)
        in a <**> Just 2 `shouldBe` Just 20

  it "implements applicative for Maybe" $ do
    ((*) <$> Just 10) <**> Just 2 `shouldBe` Just 20
    ((*) <$> Just 10) <**> Nothing `shouldBe` Nothing
    ((*) <$> Nothing) <**> Just 2 `shouldBe` Nothing
    ((*) <$> Nothing) <**> Nothing `shouldBe` Nothing

  it "can apply the internal function using fmap" $ do
    let a = fmap (*) (Just 10)
        in fmap (\f -> f 2) a `shouldBe` Just 20

  it "uses pure" $ do
     pure ("hey " ++) <*> pure "Joe"  `shouldBe` (pure "hey Joe" :: Maybe String)


  it "lifts a function" $ do
    let f a b c = (a,b,c) in
      pure f <*> [1,2,3] <*> ["a", "b", "c"] <*> ['x', 'y'] `shouldBe` [(1,"a",'x'), (1,"a",'y'), (1,"b",'x'), (1,"b",'y') ,(1,"c",'x'), (1,"c",'y')
                                                                       ,(2,"a",'x'), (2,"a",'y'), (2,"b",'x'), (2,"b",'y'), (2,"c",'x'), (2,"c",'y')
                                                                       ,(3,"a",'x'), (3,"a",'y'), (3,"b",'x'), (3,"b",'y'), (3,"c",'x'), (3,"c",'y')]

  it "shows that fmap and <$> are synomyms" $ do
    let f a b c = (a,b,c) in
      fmap f     [1,2,3] <*> ["a", "b", "c"] <*> ['x', 'y'] `shouldBe`
           f <$> [1,2,3] <*> ["a", "b", "c"] <*> ['x', 'y']


  it "implements Applicative for lists" $ do
    let f a b = (a,b) in
      pure' f <**> [1,2,3] <**> ["a"] `shouldBe`
      pure' f <*> [1,2,3] <*> ["a"]

  it "implements Applicative for IO" $ do
    let f a b = (a,b)
        fIO = pure f :: IO (String -> String -> (String, String))
    res1 <- fIO  <*> pure "Hey" <*> pure "Joe"
    res2 <- pure f <**> pure "Hey" <**> pure "Joe"
    res1 `shouldBe` res2

  it "implements Applicative for functions" $ do
    let f1 = pure (++"!") <**> (++"Joe")
        f2 = (++"Joe!")
        r1 = f1 "Hey "
        r2 = f2 "Hey " in
      r1 `shouldBe` r2

  it "implements Applicative for ZipLists" $ do
    let z1 = ZipList' [1,2,3] :: ZipList' Int
        z2 = ZipList' ["a", "b", "c"] :: ZipList' String
        in fmap (,) z1 <**> z2 `shouldBe` ZipList' [(1, "a"), (2, "b"), (3, "c")]
