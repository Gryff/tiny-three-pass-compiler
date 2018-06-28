module CompilerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Compiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "compile" $ do
    it "generates correct instructions" $ do
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

