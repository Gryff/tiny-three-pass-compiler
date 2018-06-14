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
      pass3 (Arg 1) `shouldBe` ["AR 1"]

    it "adds simple stuff" $ do
      pass3 (Add (Imm 3) (Arg 0)) `shouldBe` ["IM 3", "SW", "AR 0", "AD"]

