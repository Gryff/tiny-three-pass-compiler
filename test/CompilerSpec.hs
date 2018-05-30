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
      pass1 "[] 1 - 2 - 3" `shouldBe` (Sub (Imm 1) (Sub (Imm 2) (Imm 3)))
      pass1 "[] 1 - 2 - 3 - 4" `shouldBe` (Sub (Imm 1) (Sub (Imm 2) (Sub (Imm 3) (Imm 4))))

    it "deals with addition (no arguments)" $ do
      pass1 "[] 1 + 2" `shouldBe` (Add (Imm 1) (Imm 2))
      pass1 "[] 1 + 2 + 3" `shouldBe` (Add (Imm 1) (Add (Imm 2) (Imm 3)))

    it "deals with multiplication (no arguments)" $ do
      pass1 "[] 1 * 2" `shouldBe` (Mul (Imm 1) (Imm 2))
      pass1 "[] 1 * 2 * 3" `shouldBe` (Mul (Imm 1) (Mul (Imm 2) (Imm 3)))

    it "can add and subtract in the same expression" $ do
      pass1 "[] 1 - 2 + 3" `shouldBe` (Sub (Imm 1) (Add (Imm 2) (Imm 3)))

