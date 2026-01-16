module Main where

import Parser.SpecDecl (declSpec)
import Parser.SpecExpr (exprSpec)
import Parser.SpecLValue (lvalueSpec)
import Parser.SpecLitterals (litteralsSpec)
import Parser.SpecOperator (operatorSpec)
import Parser.SpecStmt (stmtSpec)
import Parser.SpecTypes (typesSpec)
import Parser.SpecUtils (utilsSpec)
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
  -- Parser module tests
  utilsSpec
  operatorSpec
  litteralsSpec
  typesSpec
  exprSpec
  lvalueSpec
  stmtSpec
  declSpec
