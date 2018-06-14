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

