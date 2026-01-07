module Main where

import SpecLispAST (specLispAST)
import SpecLispSexprtoAST (specSexprtoAST)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  specLispAST
  specSexprtoAST
