{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Lisp-Interpreter Specs
-}

module Main (main) where

import LispAstSpec (astModuleSpec)
import LispBootstrapSpec (bootstrapModuleSpec)
import SexprtoASTSpec
import SpecParser
import Test.Hspec

main :: IO ()
main = hspec $ do
  parserSpec
  bootstrapModuleSpec
  astModuleSpec
  SexprtoASTSpec.spec
