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

    it "multiplies simple stuff" $ do
      pass3 (Mul (Imm 3) (Arg 0)) `shouldBe` ["IM 3", "SW", "AR 0", "MU"]

    it "2*3 + 4*5" $ do
      pass3 (Add (Mul (Imm 2) (Imm 3)) (Mul (Imm 4) (Imm 5))) `shouldBe` ["IM 2", "SW", "IM 3", "MU", "PU", "IM 4", "SW", "IM 5", "MU", "SW", "PO", "AD"]

    it "simple subtraction" $ do
      pass3 (Sub (Arg 0) (Arg 1)) `shouldBe` ["AR 1", "SW", "AR 0", "SU"]

    it "simple division" $ do
      pass3 (Div (Arg 0) (Arg 1)) `shouldBe` ["AR 1", "SW", "AR 0", "DI"]

