import Data.List
-- import Data.List (nub, sort)
-- import Data.List hiding (sort)

-- numUniques = length . nub

-- import qualified Data.List (nub)
-- numUniques = length . Data.List.nub

--import qualified Data.List as M
-- numUniques = length . M.nub


-- without the import statement, this function doesn't compile,
-- as nub is defined in a module different than Prelude
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- :browse Data.List from ghci lists the exported functions


-- Data.List
-- intersperse
tryIntersperse = intersperse '.' "GIMP" == "G.I.M.P"

myIntersperse :: a -> [a] -> [a]
myIntersperse separator list = case list of
  [] -> []
  [a] -> [a]
  (x:xs) -> x : separator : (myIntersperse separator xs)
