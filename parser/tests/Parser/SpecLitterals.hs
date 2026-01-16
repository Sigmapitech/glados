{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecLitterals (litteralsSpec) where

import AST.Types.Common (Located (..), unLocated)
import AST.Types.Literal
  ( ArrayLiteral (..),
    BoolLiteral (..),
    FloatLiteral (..),
    IntBase (..),
    IntLiteral (..),
    Literal (..),
    StringLiteral (..),
  )
import Data.Text (Text)
import Error (ParseError)
import Parser.Literal
import Parser.Utils (TokenParser, voidSpann)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runLiteralParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runLiteralParser parser = runParser parser "<test>"

mockExprParser :: TokenParser (Located Text)
mockExprParser = return (Located voidSpann "mock")

-- Helper to check if result is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- Helper to extract Right value
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight called on Left"

-- Helper to create Located tokens
loc :: TokenContent -> Token
loc = Located voidSpann

litteralsSpec :: Spec
litteralsSpec = do
  describe "Parser.Literal - parseStringLiteral" $ do
    it "parses a simple string" $ do
      let tokens = [loc (TokString "hello")]
      let result = runLiteralParser parseStringLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (StringLiteral str) = fromRight result
      str `shouldBe` "hello"

    it "parses an empty string" $ do
      let tokens = [loc (TokString "")]
      let result = runLiteralParser parseStringLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (StringLiteral str) = fromRight result
      str `shouldBe` ""

    it "parses a string with escapes" $ do
      let tokens = [loc (TokString "hello\\nworld")]
      let result = runLiteralParser parseStringLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (StringLiteral str) = fromRight result
      str `shouldBe` "hello\\nworld"

  describe "Parser.Literal - parseBoolLiteral" $ do
    it "parses True" $ do
      let tokens = [loc (TokBool True)]
      let result = runLiteralParser parseBoolLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (BoolLiteral b) = fromRight result
      b `shouldBe` True

    it "parses False" $ do
      let tokens = [loc (TokBool False)]
      let result = runLiteralParser parseBoolLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (BoolLiteral b) = fromRight result
      b `shouldBe` False

  describe "Parser.Literal - parseIntLiteral" $ do
    it "parses a positive decimal integer" $ do
      let tokens = [loc (TokInt 42 BaseDec)]
      let result = runLiteralParser parseIntLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (IntLiteral base val) = fromRight result
      base `shouldBe` BaseDec
      val `shouldBe` 42

    it "parses a negative decimal integer" $ do
      let tokens = [loc (TokSymbol "-"), loc (TokInt 42 BaseDec)]
      let result = runLiteralParser parseIntLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (IntLiteral base val) = fromRight result
      base `shouldBe` BaseDec
      val `shouldBe` (-42)

    it "parses zero" $ do
      let tokens = [loc (TokInt 0 BaseDec)]
      let result = runLiteralParser parseIntLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (IntLiteral base val) = fromRight result
      base `shouldBe` BaseDec
      val `shouldBe` 0

    it "parses a hexadecimal integer" $ do
      let tokens = [loc (TokInt 255 BaseHex)]
      let result = runLiteralParser parseIntLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (IntLiteral base val) = fromRight result
      base `shouldBe` BaseHex
      val `shouldBe` 255

    it "parses an octal integer" $ do
      let tokens = [loc (TokInt 63 BaseOct)]
      let result = runLiteralParser parseIntLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (IntLiteral base val) = fromRight result
      base `shouldBe` BaseOct
      val `shouldBe` 63

    it "parses a binary integer" $ do
      let tokens = [loc (TokInt 15 BaseBin)]
      let result = runLiteralParser parseIntLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (IntLiteral base val) = fromRight result
      base `shouldBe` BaseBin
      val `shouldBe` 15

    it "parses with explicit plus sign" $ do
      let tokens = [loc (TokSymbol "+"), loc (TokInt 42 BaseDec)]
      let result = runLiteralParser parseIntLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (IntLiteral base val) = fromRight result
      base `shouldBe` BaseDec
      val `shouldBe` 42

  describe "Parser.Literal - parseFloatLiteral" $ do
    it "parses a simple float" $ do
      let tokens = [loc (TokInt 3 BaseDec), loc (TokSymbol "."), loc (TokInt 14 BaseDec)]
      let result = runLiteralParser parseFloatLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (FloatLiteral val) = fromRight result
      val `shouldBe` 3.14

    it "parses zero float" $ do
      let tokens = [loc (TokInt 0 BaseDec), loc (TokSymbol "."), loc (TokInt 0 BaseDec)]
      let result = runLiteralParser parseFloatLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (FloatLiteral val) = fromRight result
      val `shouldBe` 0.0

    it "parses float with large fractional part" $ do
      let tokens = [loc (TokInt 1 BaseDec), loc (TokSymbol "."), loc (TokInt 234567 BaseDec)]
      let result = runLiteralParser parseFloatLiteral tokens
      result `shouldSatisfy` isRight
      let Located _ (FloatLiteral val) = fromRight result
      val `shouldBe` 1.234567

  describe "Parser.Literal - parseArrayLiteral" $ do
    it "parses an empty array" $ do
      let tokens = [loc (TokSymbol "["), loc (TokSymbol "]")]
      let result = runLiteralParser (parseArrayLiteral parseIntLiteral) tokens
      result `shouldSatisfy` isRight
      let Located _ (ArrayLiteral elems) = fromRight result
      length elems `shouldBe` 0

    it "parses an array with one element" $ do
      let tokens = [loc (TokSymbol "["), loc (TokInt 42 BaseDec), loc (TokSymbol "]")]
      let mockParser = parseIntLiteral
      let result = runLiteralParser (parseArrayLiteral mockParser) tokens
      result `shouldSatisfy` isRight
      let Located _ (ArrayLiteral elems) = fromRight result
      length elems `shouldBe` 1

    it "parses an array with multiple elements" $ do
      let tokens =
            [ loc (TokSymbol "["),
              loc (TokInt 1 BaseDec),
              loc (TokSymbol ","),
              loc (TokInt 2 BaseDec),
              loc (TokSymbol ","),
              loc (TokInt 3 BaseDec),
              loc (TokSymbol "]")
            ]
      let mockParser = parseIntLiteral
      let result = runLiteralParser (parseArrayLiteral mockParser) tokens
      result `shouldSatisfy` isRight
      let Located _ (ArrayLiteral elems) = fromRight result
      length elems `shouldBe` 3

  describe "Parser.Literal - parseLiteral" $ do
    it "parses integer literals" $ do
      let tokens = [loc (TokInt 42 BaseDec)]
      let result = runLiteralParser (parseLiteral mockExprParser) tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LitInt _ -> True `shouldBe` True
        _ -> fail "Expected LitInt"

    it "parses float literals" $ do
      let tokens = [loc (TokInt 3 BaseDec), loc (TokSymbol "."), loc (TokInt 14 BaseDec)]
      let result = runLiteralParser (parseLiteral mockExprParser) tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LitFloat _ -> True `shouldBe` True
        _ -> fail "Expected LitFloat"

    it "parses bool literals" $ do
      let tokens = [loc (TokBool True)]
      let result = runLiteralParser (parseLiteral mockExprParser) tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LitBool _ -> True `shouldBe` True
        _ -> fail "Expected LitBool"

    it "parses string literals" $ do
      let tokens = [loc (TokString "hello")]
      let result = runLiteralParser (parseLiteral mockExprParser) tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LitString _ -> True `shouldBe` True
        _ -> fail "Expected LitString"

    it "parses array literals" $ do
      let tokens = [loc (TokSymbol "["), loc (TokSymbol "]")]
      let result = runLiteralParser (parseLiteral mockExprParser) tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LitArray _ -> True `shouldBe` True
        _ -> fail "Expected LitArray"
