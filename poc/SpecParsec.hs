module Main (main) where

import ParsecPoC (SExpr (..), parseString)
import Test.Hspec

dummySpec :: Spec
dummySpec = do
  describe "dummy test" $ do
    it "Should always pass" $ do
      True `shouldBe` True

validInputSpec :: Spec
validInputSpec = do
  describe "valid input test" $ do
    it "Should parse valid input correctly" $ do
      let input = "(define x 42)"
      result <- parseString input
      result `shouldBe` [List [AtomSym "define", AtomSym "x", AtomInt 42]]

invalidInputSpec :: Spec
invalidInputSpec = do
  describe "invalid input test" $ do
    it "Should fail on invalid input" $ do
      let input = "(define x 42" -- Missing closing parenthesis
      parseString input `shouldThrow` anyException

main :: IO ()
main = hspec $ do
  dummySpec
  validInputSpec
  invalidInputSpec
