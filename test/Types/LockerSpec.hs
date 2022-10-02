module Types.LockerSpec where

import Test.Hspec
import qualified Data.Map as Map

data LockerState = Taken | Free

-- separating data type definition and derived instances
-- requires ghc-options: -XStandaloneDeriving in the cabal file
deriving instance Show LockerState
deriving instance Eq LockerState

type Code = String
type Number = Int
type Error = String

type LockerMap = Map.Map Number (LockerState, Code)

lockerLookup :: Number -> LockerMap -> Either Error Code
lockerLookup lockerNumber lockers =
  case Map.lookup lockerNumber lockers of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state == Free
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


spec :: Spec
spec = do
  it "" $ do
    let lockers =
          Map.fromList $
          [ (1, (Free,  "1771"))
          , (2, (Free,  "8882"))
          , (3, (Taken, "1010"))] in
      do lockerLookup 1 lockers `shouldBe` Right "1771"
         lockerLookup 3 lockers `shouldBe` Left "Locker 3 is already taken!"
         lockerLookup 4 lockers `shouldBe` Left "Locker 4 doesn't exist!"
