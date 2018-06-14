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

