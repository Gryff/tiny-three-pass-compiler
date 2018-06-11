module CompilerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Compiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pass1" $ do
    it "returns statements that need no work" $ do
      pass1 "[] 1" `shouldBe` (Imm 1)
      pass1 "[] 3" `shouldBe` (Imm 3)

    it "deals with simple subtraction (no arguments)" $ do
      pass1 "[] 1 - 2" `shouldBe` (Sub (Imm 1) (Imm 2))

    it "deals with double subtraction (no arguments)" $ do
      pass1 "[] 1 - 2 - 3" `shouldBe` (Sub (Sub (Imm 1) (Imm 2)) (Imm 3))
      pass1 "[] 1 - 2 - 3 - 4" `shouldBe` (Sub (Sub (Sub (Imm 1) (Imm 2)) (Imm 3)) (Imm 4))

    it "deals with addition (no arguments)" $ do
      pass1 "[] 1 + 2" `shouldBe` (Add (Imm 1) (Imm 2))
      pass1 "[] 1 + 2 + 3" `shouldBe` (Add (Add (Imm 1) (Imm 2)) (Imm 3))

    it "deals with multiplication (no arguments)" $ do
      pass1 "[] 1 * 2" `shouldBe` (Mul (Imm 1) (Imm 2))
      pass1 "[] 1 * 2 * 3" `shouldBe` (Mul (Mul (Imm 1) (Imm 2)) (Imm 3))

    it "deals with division (no arguments)" $ do
      pass1 "[] 1 / 2" `shouldBe` (Div (Imm 1) (Imm 2))
      pass1 "[] 1 / 2 / 3" `shouldBe` (Div (Div (Imm 1) (Imm 2)) (Imm 3))

    it "can add and subtract in the same expression" $ do
      pass1 "[] 1 - 2 + 3" `shouldBe` (Sub (Imm 1) (Add (Imm 2) (Imm 3)))

    it "can mix all the operations" $ do
      pass1 "[] 1 - 2 + 3 * 4 / 5" `shouldBe` (Sub (Imm 1) (Add (Imm 2) (Mul (Imm 3) (Div (Imm 4) (Imm 5)))))

    it "can return arguments" $ do
      pass1 "[x] x" `shouldBe` (Arg 0)
      pass1 "[x y] x" `shouldBe` (Arg 0)
      pass1 "[x y] y" `shouldBe` (Arg 1)

    it "can use arguments in the expressions" $ do
      pass1 "[x] x + 1" `shouldBe` (Add (Arg 0) (Imm 1))
      pass1 "[zz] zz * 8" `shouldBe` (Mul (Arg 0) (Imm 8))

    it "can use arguments with other arguments in the expressions" $ do
      pass1 "[x y] x + y" `shouldBe` (Add (Arg 0) (Arg 1))
      pass1 "[x y z] z + y - 1" `shouldBe` (Sub (Add (Arg 2) (Arg 1)) (Imm 1))

    it "can deal with parens" $ do
      pass1 "[x y] (x + y) / 2" `shouldBe` (Div (Add (Arg 0) (Arg 1)) (Imm 2))

    it "supports multiple digits?" $ do
      pass1 "[] 11" `shouldBe` (Imm 11)
