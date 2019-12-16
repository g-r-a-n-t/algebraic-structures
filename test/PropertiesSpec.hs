{-# LANGUAGE DataKinds #-}
module PropertiesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Modular
import AlgebraicStructures.Properties
import AlgebraicStructures.Base

n7 = [0..6] :: [Mod Integer 7]
n7' = [1..6] :: [Mod Integer 7]
n0_7 = [0..7] :: [Integer]

spec :: Spec
spec = do
  describe "Closure verification" $ do
    it "returns true for integers mod 7 over addition." $ do
      ver (Closure (+) n7) `shouldBe` True
    it "returns false for integers 0 through 7 over addition." $ do
      ver (Closure (+) n0_7) `shouldBe` False

  describe "Associativity verification" $ do
    it "returns true for integers 0 through 7 over addition." $ do
      ver (Associativity (+) n0_7) `shouldBe` True
    it "returns false for integers 0 through 7 over exponentiation." $ do
      ver (Associativity (^) n0_7) `shouldBe` False

  describe "Commutativity verification" $ do
    it "returns true for integers 0 through 7 over addition." $ do
      ver (Commutativity (+) n0_7) `shouldBe` True
    it "returns false for integers 0 through 7 over exponentiation." $ do
      ver (Commutativity (^) n0_7) `shouldBe` False

  describe "Identity verification" $ do
    it "returns true for integers mod 7 over addition." $ do
      ver (Identity (+) n7) `shouldBe` True
    it "returns false for integers mod 7 without 0 over addition." $ do
      ver (Identity (+) n7') `shouldBe` False

  describe "Invertibility verification" $ do
    it "returns true for integers mod 7 over addition." $ do
      ver (Invertibility (+) n7) `shouldBe` True
    it "returns false for integers mod 7 over multiplication." $ do
      ver (Invertibility (*) n7) `shouldBe` False

  describe "Distributivity verification" $ do
    it "returns true for integers mod 7 over addition and multiplication." $ do
      ver (Distributivity (+) (*) n7) `shouldBe` True
    it "returns false for integers mod 7 over addition and exponentiation." $ do
      ver (Distributivity (+) (^) n7) `shouldBe` False