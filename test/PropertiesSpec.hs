{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module PropertiesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Properties
import Data.Modular
import Base
import Properties

instance Add (Mod Integer 7) where
  add = (+)

n7 = [0..6] :: [Mod Integer 7]

spec :: Spec
spec = do
  describe "AddClosed" $ do
    it "returns true for integers mod 7 over addition." $ do
      ver (AddClosed n7) `shouldBe` True
