module Main where

import SpecComplexExamples (specComplexExamples)
import SpecEvaluator (specEvaluator)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  specComplexExamples
  specEvaluator
