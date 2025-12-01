{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Lisp-Interpreter Specs
-}

module Main (main) where

import LispBootstrapSpec (bootstrapModuleSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  bootstrapModuleSpec
