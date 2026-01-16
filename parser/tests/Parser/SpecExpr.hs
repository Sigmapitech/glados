{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecExpr (exprSpec) where

import AST.Types.AST (Expr (..))
import AST.Types.Common (FieldName (..), Located (..), unLocated)
import AST.Types.Literal (IntBase (..))
import Error (ParseError)
import Parser.Expr
import Parser.Utils (TokenParser, voidSpann)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runExprParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runExprParser parser = runParser parser "<test>"

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

exprSpec :: Spec
exprSpec = do
  describe "Parser.Expr - parseExprVar" $ do
    it "parses a simple variable" $ do
      let tokens = [loc (TokIdentifier "x")]
      let result = runExprParser parseExprVar tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprVar _ -> True `shouldBe` True
        _ -> fail "Expected ExprVar"

    it "parses a multi-character variable" $ do
      let tokens = [loc (TokIdentifier "myVar")]
      let result = runExprParser parseExprVar tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprVar _ -> True `shouldBe` True
        _ -> fail "Expected ExprVar"

  describe "Parser.Expr - parseExprPrimary" $ do
    it "parses integer literal" $ do
      let tokens = [loc (TokInt 42 BaseDec)]
      let result = runExprParser parseExprPrimary tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprLiteral _ -> True `shouldBe` True
        _ -> fail "Expected ExprLiteral"

    it "parses boolean literal" $ do
      let tokens = [loc (TokBool True)]
      let result = runExprParser parseExprPrimary tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprLiteral _ -> True `shouldBe` True
        _ -> fail "Expected ExprLiteral"

    it "parses string literal" $ do
      let tokens = [loc (TokString "hello")]
      let result = runExprParser parseExprPrimary tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprLiteral _ -> True `shouldBe` True
        _ -> fail "Expected ExprLiteral"

    it "parses parenthesized expression" $ do
      let tokens = [loc (TokSymbol "("), loc (TokInt 42 BaseDec), loc (TokSymbol ")")]
      let result = runExprParser parseExprPrimary tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprParen _ -> True `shouldBe` True
        _ -> fail "Expected ExprParen"

    it "parses variable reference" $ do
      let tokens = [loc (TokIdentifier "x")]
      let result = runExprParser parseExprPrimary tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprVar _ -> True `shouldBe` True
        _ -> fail "Expected ExprVar"

  describe "Parser.Expr - parseExpr" $ do
    it "parses simple addition" $ do
      let tokens = [loc (TokInt 2 BaseDec), loc (TokSymbol "+"), loc (TokInt 3 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses simple multiplication" $ do
      let tokens = [loc (TokInt 4 BaseDec), loc (TokSymbol "*"), loc (TokInt 8 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses precedence correctly (multiplication before addition)" $ do
      let tokens = [loc (TokInt 2 BaseDec), loc (TokSymbol "+"), loc (TokInt 3 BaseDec), loc (TokSymbol "*"), loc (TokInt 4 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary _ (Located _ (ExprLiteral _)) (Located _ (ExprBinary {})) -> True `shouldBe` True
        _ -> fail "Expected correct precedence structure"

    it "parses unary negation" $ do
      let tokens = [loc (TokSymbol "-"), loc (TokInt 42 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprUnary _ _ -> True `shouldBe` True
        _ -> fail "Expected ExprUnary"

    it "parses logical NOT" $ do
      let tokens = [loc (TokSymbol "!"), loc (TokBool True)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprUnary _ _ -> True `shouldBe` True
        _ -> fail "Expected ExprUnary"

  describe "Parser.Expr - parseFieldInit" $ do
    it "parses field initialization" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol "="), loc (TokInt 10 BaseDec)]
      let result = runExprParser parseFieldInit tokens
      result `shouldSatisfy` isRight
      let (fieldName, _) = fromRight result
      case unLocated fieldName of
        FieldName "x" -> True `shouldBe` True
        _ -> fail "Expected FieldName x"
