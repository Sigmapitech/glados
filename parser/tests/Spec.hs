module Main where

import SpecError (errorSpec)
import SpecLexer (lexerSpec)
import SpecLib (libSpec)
import SpecTokens (tokensSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  errorSpec
  tokensSpec
  lexerSpec
  libSpec
