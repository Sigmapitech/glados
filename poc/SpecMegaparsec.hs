module Main (main) where

import MegaparsecPoC (SExpr (..), parseString)
import System.IO (stderr)
import System.IO.Silently (hSilence)
import Test.Hspec

wrapper :: String -> IO [SExpr]
wrapper = hSilence [stderr] . parseString

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
      result <- wrapper input
      result `shouldBe` [List [AtomSym "define", AtomSym "x", AtomInt 42]]

invalidInputSpec :: Spec
invalidInputSpec = do
  describe "invalid input test" $ do
    it "Should fail on invalid input" $ do
      let input = "(define x 42" -- Missing closing parenthesis
      wrapper input `shouldThrow` anyException

main :: IO ()
main = hspec $ do
  dummySpec
  validInputSpec
  invalidInputSpec
