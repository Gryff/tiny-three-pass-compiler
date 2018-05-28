module CompilerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Compiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pass1" $ do
    it "should return statements" $ do
      pass1 "[] 1" `shouldBe` (Imm 1)

