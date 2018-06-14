module Pass2Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Compiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pass2" $ do
    it "returns values or args that can't be reduced" $ do
      pass2 (Imm 2) `shouldBe` (Imm 2)
      pass2 (Arg 1) `shouldBe` (Arg 1)
      
    it "reduces simple multiplication" $ do
      pass2 (Mul (Imm 2) (Imm 5)) `shouldBe` (Imm 10)

    it "doesn't reduce simple multiplication with args" $ do
      pass2 (Mul (Arg 0) (Imm 5)) `shouldBe` (Mul (Arg 0) (Imm 5))
      pass2 (Mul (Imm 1) (Arg 0)) `shouldBe` (Mul (Imm 1) (Arg 0))
      pass2 (Mul (Arg 1) (Arg 0)) `shouldBe` (Mul (Arg 1) (Arg 0))
      pass2 (Mul (Mul (Imm 2) (Imm 5)) (Arg 0)) `shouldBe` (Mul (Imm 10) (Arg 0))

    it "reduces nested multiplications" $ do
      pass2 (Mul (Mul (Imm 2) (Imm 5)) (Imm 3)) `shouldBe` (Imm 30)
      pass2 (Mul (Imm 2) (Mul (Imm 5) (Imm 3))) `shouldBe` (Imm 30)
      pass2 (Mul (Mul (Mul (Imm 2) (Imm 5)) (Imm 3)) (Imm 10)) `shouldBe` (Imm 300)

    it "reduces a mix of values and arguments (multiplication)" $ do
      pass2 (Mul (Mul (Imm 2) (Imm 5)) (Arg 0)) `shouldBe` (Mul (Imm 10) (Arg 0))

    it "reduces simple subtraction" $ do
      pass2 (Sub (Imm 5) (Imm 2)) `shouldBe` (Imm 3)

    it "reduces simple addition" $ do
      pass2 (Add (Imm 2) (Imm 5)) `shouldBe` (Imm 7)

    it "reduces simple division" $ do
      pass2 (Div (Imm 10) (Imm 5)) `shouldBe` (Imm 2)

