module Main (main) where

import SpecIntegration (specIntegration)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  specIntegration
