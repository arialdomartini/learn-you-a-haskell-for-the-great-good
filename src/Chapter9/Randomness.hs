module Chapter9.Randomness where
import System.Random

getRandomNumber :: (Int, StdGen)
getRandomNumber = random $ mkStdGen 100

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen1 = (first, second, third) where
  (first, gen2) = random gen1
  (second, gen3) = random gen2
  (third, _) = random gen3

infiniteCoins :: StdGen -> [Bool]
infiniteCoins gen = randoms gen

infiniteCustomCoins :: StdGen -> [Bool]
infiniteCustomCoins gen = randoms' gen

randoms' :: StdGen -> [Bool]
randoms' gen =
  let (rnd, newGen) = random gen
  in rnd : (randoms' newGen)

getRandomRange :: (Int, Int) -> StdGen -> [Int]
getRandomRange (from, to) gen = randomRs' (from, to) gen

randomRs' :: (Int, Int) -> StdGen -> [Int]
randomRs' (from, to) gen = rndNormalized : randomRs' (from, to) gen'
  where (rnd, gen') = random gen
        rndNormalized = (rnd `mod` (to - from)) + from
