module Main (main) where

import Test.Hspec

dummySpec :: Spec
dummySpec = do
  describe "dummy test" $ do
    it "Should always pass" $ do
      True `shouldBe` True

main :: IO ()
main = hspec $ do
  dummySpec
