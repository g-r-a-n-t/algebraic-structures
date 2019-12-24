module MappingsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import AlgebraicStructures.Base
import AlgebraicStructures.Mappings

inA = [1..3]
inB = ["A", "B", "C", "D"]
inF x
  | x == 1 = "A"
  | x == 2 = "B"
  | x == 3 = "C"

surA = [1..4]
surB = ["A", "B", "C"]
surF x
  | x == 1 = "A"
  | x == 2 = "B"
  | x == 3 = "C"
  | x == 4 = "C"

biA = [1..4]
biB = ["A", "B", "C", "D"]
biF x
  | x == 1 = "D"
  | x == 2 = "B"
  | x == 3 = "C"
  | x == 4 = "A"

spec :: Spec
spec = do
  describe "Injective verification" $ do
    it "returns true for injective only mapping." $ do
      ver (Injective inF inA inB) `shouldBe` True
    it "returns false for surjective only mapping." $ do
      ver (Injective surF surA surB) `shouldBe` False
    it "returns true for bijective mapping." $ do
      ver (Injective biF biA biB) `shouldBe` True
  describe "Surjective verification" $ do
    it "returns false for injective only mapping." $ do
      ver (Surjective inF inA inB) `shouldBe` False
    it "returns true for surjective only mapping." $ do
      ver (Surjective surF surA surB) `shouldBe` True
    it "returns true for bijective mapping." $ do
      ver (Surjective biF biA biB) `shouldBe` True
  describe "Bijective verification" $ do
    it "returns false for injective only mapping." $ do
      ver (Bijective inF inA inB) `shouldBe` False
    it "returns false for surjective only mapping." $ do
      ver (Bijective surF surA surB) `shouldBe` False
    it "returns true for bijective mapping." $ do
      ver (Bijective biF biA biB) `shouldBe` True
