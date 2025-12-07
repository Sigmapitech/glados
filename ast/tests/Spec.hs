module Main where

import SpecAST (specAST)
import SpecSexprtoAST (spec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  specAST
  spec
