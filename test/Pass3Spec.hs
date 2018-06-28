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

    it "(-1)+(-1) - 0*(-1)" $ do
      pass3 (Sub (Add (Imm (-1)) (Imm (-1))) (Mul (Imm 0) (Imm (-1)))) `shouldBe` ["IM -1","SW","IM -1","AD","PU","IM 0","SW","IM -1","MU","SW","PO","SU"]

    it "((-1)*2) / (2*(-2))" $ do
      pass3 (Div (Mul (Imm (-1)) (Imm 2)) (Mul (Imm 2) (Imm (-2)))) `shouldBe` ["IM -1","SW","IM 2","MU","PU","IM 2","SW","IM -2","MU","SW","PO","DI"]

    it "(xx + yy) / 2" $ do
      pass3 (Div (Add (Arg 0) (Arg 1)) (Imm 2)) `shouldBe` ["AR 0","SW","AR 1","AD","PU","IM 2","SW","PO","DI"]

    it "er..." $ do
      simulate (compile "[ xx yy ] (xx + yy) / 2") [1, -1] `shouldBe` 0

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0,_,_) = r0

