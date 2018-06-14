module Pass3Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Compiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pass3" $ do
    it "puts value ASTs in the register" $ do
      pass3 (Imm 2) `shouldBe` ["IM 2"]

