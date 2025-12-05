module Main (main) where

import LispAstSpec (astModuleSpec)
import LispBootstrapSpec (bootstrapModuleSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  bootstrapModuleSpec
  astModuleSpec
