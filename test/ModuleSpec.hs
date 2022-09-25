module ModuleSpec where

import Test.Hspec          -- this imports a whole module
import Data.List (nub)     -- this imports a single function

import qualified Text.Read                  -- this imports the whole module, requiring the fully-qualified name
import qualified Data.List as L             -- this imports the whole module, requiring an alias
import qualified Data.Maybe as M (isJust)  -- this imports a single function, requiring an alias


spec :: Spec
spec = do
  it "can use an imported function" $ do
    nub ['a','b','b'] `shouldBe` ['a','b']

  it "uses a qualified module" $ do
    Text.Read.read "'a'" `shouldBe` 'a'
    not True `shouldBe` False
    M.isJust (Just 12 :: Maybe Int) `shouldBe` True
    L.elem 'a' ['a', 'b'] `shouldBe` True
