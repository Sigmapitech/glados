module Main (main) where

import LispBootstrapSpec (bootstrapModuleSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  bootstrapModuleSpec
