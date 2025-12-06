module Main (main) where

import ComplexExamplesSpec (spec)
import EvaluatorSpec (spec)
import LispAstSpec (astModuleSpec)
import LispBootstrapSpec (bootstrapModuleSpec)
import SexprtoASTSpec (spec)
import SpecParser (parserSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  parserSpec
  bootstrapModuleSpec
  astModuleSpec
  EvaluatorSpec.spec
  ComplexExamplesSpec.spec
  SexprtoASTSpec.spec
