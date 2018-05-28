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

