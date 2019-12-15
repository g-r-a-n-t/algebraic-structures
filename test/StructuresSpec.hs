{-# LANGUAGE DataKinds #-}
module StructuresSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Modular
import AlgebraicStructures.Structures
import AlgebraicStructures.Base

n7 = [0..6] :: [Mod Integer 7]

spec :: Spec
spec = do
  describe "Abelian group verification" $ do
    it "returns true for the integers mod 7 over addition." $ do
      ver (AbelianGroup (+) n7) `shouldBe` True
    it "returns false for the integers mod 7 over multiplication." $ do
      ver (AbelianGroup (*) n7) `shouldBe` False
