{-# LANGUAGE DataKinds #-}
module PropertiesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Modular
import AlgebraicStructures.Properties
import AlgebraicStructures.Base

n7 = [0..6] :: [Mod Integer 7]
n0_7 = [0..7] :: [Integer]

spec :: Spec
spec = do
  describe "Closed Addition" $ do
    it "returns true for integers mod 7 over addition." $ do
      ver (Closure (+) n7) `shouldBe` True
    it "returns false for integers 0 through 7 over addition." $ do
      ver (Closure (+) n0_7) `shouldBe` False

  describe "Associative Addition" $ do
    it "returns true for integers 0 through 7 over addition." $ do
      ver (Associativity (+) n0_7) `shouldBe` True
    it "returns false for integers 0 through 7 over exponentiation." $ do
      ver (Associativity (^) n0_7) `shouldBe` False

  describe "Commutative Addition" $ do
    it "returns true for integers 0 through 7 over addition." $ do
      ver (Commutativity (+) n0_7) `shouldBe` True
    it "returns false for integers 0 through 7 over exponentiation." $ do
      ver (Commutativity (^) n0_7) `shouldBe` False
