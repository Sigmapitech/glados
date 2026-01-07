module Main where

import SpecAST (specAST)
import Test.Hspec (hspec)

main :: IO ()
main = hspec specAST
