module Main (main) where

import LispAstSpec (astModuleSpec)
import LispBootstrapSpec (bootstrapModuleSpec)
import SexprtoASTSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  bootstrapModuleSpec
  astModuleSpec
  SexprtoASTSpec.spec
