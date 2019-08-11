module Chapter9.Randomness where
import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen1 = (first, second, third) where
  (first, gen2) = random gen1
  (second, gen3) = random gen2
  (third, _) = random gen3

getRandomNumber :: (Int, StdGen)
getRandomNumber = random $ mkStdGen 100
