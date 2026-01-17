{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecExpr (exprSpec) where

import AST.Types.AST (Expr (..))
import AST.Types.Common (Located (..), unLocated)
import AST.Types.Literal (IntBase (..))
import Error (ParseError)
import Parser.Expr
  ( parseExpr,
    parseExprCall,
    parseExprCast,
    parseExprLiteral,
    parseExprParen,
    parseExprVar,
  )
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
      let result = runExprParser parseExprLiteral tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprLiteral _ -> True `shouldBe` True
        _ -> fail "Expected ExprLiteral"

    it "parses boolean literal" $ do
      let tokens = [loc (TokBool True)]
      let result = runExprParser parseExprLiteral tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprLiteral _ -> True `shouldBe` True
        _ -> fail "Expected ExprLiteral"

    it "parses string literal" $ do
      let tokens = [loc (TokString "hello")]
      let result = runExprParser parseExprLiteral tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprLiteral _ -> True `shouldBe` True
        _ -> fail "Expected ExprLiteral"

    it "parses parenthesized expression" $ do
      let tokens = [loc (TokSymbol "("), loc (TokInt 42 BaseDec), loc (TokSymbol ")")]
      let result = runExprParser parseExprParen tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprParen _ -> True `shouldBe` True
        _ -> fail "Expected ExprParen"

    it "parses variable reference" $ do
      let tokens = [loc (TokIdentifier "x")]
      let result = runExprParser parseExprVar tokens
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

  describe "Parser.Expr - parseExprCall" $ do
    it "parses function call with no arguments" $ do
      let tokens = [loc (TokIdentifier "foo"), loc (TokSymbol "("), loc (TokSymbol ")")]
      let result = runExprParser parseExprCall tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprCall _ args -> length args `shouldBe` 0
        _ -> fail "Expected ExprCall"

    it "parses function call with one argument" $ do
      let tokens = [loc (TokIdentifier "foo"), loc (TokSymbol "("), loc (TokInt 42 BaseDec), loc (TokSymbol ")")]
      let result = runExprParser parseExprCall tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprCall _ args -> length args `shouldBe` 1
        _ -> fail "Expected ExprCall"

    it "parses function call with multiple arguments" $ do
      let tokens = [loc (TokIdentifier "add"), loc (TokSymbol "("), loc (TokInt 1 BaseDec), loc (TokSymbol ","), loc (TokInt 2 BaseDec), loc (TokSymbol ","), loc (TokInt 3 BaseDec), loc (TokSymbol ")")]
      let result = runExprParser parseExprCall tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprCall _ args -> length args `shouldBe` 3
        _ -> fail "Expected ExprCall"

  describe "Parser.Expr - Field Access" $ do
    it "parses simple field access" $ do
      let tokens = [loc (TokIdentifier "obj.field")]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprVar _ -> True `shouldBe` True
        _ -> fail "Expected ExprVar"

    it "parses chained field access" $ do
      let tokens = [loc (TokIdentifier "obj.field1.field2")]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprVar _ -> True `shouldBe` True
        _ -> fail "Expected ExprVar"

  -- describe "Parser.Expr - Array Index" $ do
  --   it "parses simple array index" $ do
  --     let tokens = [loc (TokIdentifier "arr"), loc (TokSymbol "["), loc (TokInt 0 BaseDec), loc (TokSymbol "]")]
  --     let result = runExprParser parseExpr tokens
  --     result `shouldSatisfy` isRight
  --     case unLocated (fromRight result) of
  --       ExprIndex _ _ -> True `shouldBe` True
  --       _ -> fail "Expected ExprIndex"

  -- it "parses chained array index" $ do
  --   let tokens = [loc (TokIdentifier "matrix"), loc (TokSymbol "["), loc (TokInt 0 BaseDec), loc (TokSymbol "]"), loc (TokSymbol "["), loc (TokInt 1 BaseDec), loc (TokSymbol "]")]
  --   let result = runExprParser parseExpr tokens
  --   result `shouldSatisfy` isRight
  --   case unLocated (fromRight result) of
  --     ExprIndex (Located _ (ExprIndex _ _)) _ -> True `shouldBe` True
  --     _ -> fail "Expected chained ExprIndex"

  -- it "parses array index with expression" $ do
  --   let tokens = [loc (TokIdentifier "arr"), loc (TokSymbol "["), loc (TokIdentifier "i"), loc (TokSymbol "+"), loc (TokInt 1 BaseDec), loc (TokSymbol "]")]
  --   let result = runExprParser parseExpr tokens
  --   result `shouldSatisfy` isRight
  --   case unLocated (fromRight result) of
  --     ExprIndex _ (Located _ (ExprBinary {})) -> True `shouldBe` True
  --     _ -> fail "Expected ExprIndex with binary expression"

  describe "Parser.Expr - Type Cast" $ do
    it "parses int cast" $ do
      let tokens = [loc (TokKeyword "int"), loc (TokSymbol "("), loc (TokIdentifier "x"), loc (TokSymbol ")")]
      let result = runExprParser parseExprCast tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprCast _ _ -> True `shouldBe` True
        _ -> fail "Expected ExprCast"

  describe "Parser.Expr - Complex Expressions" $ do
    it "parses expression with parentheses and operators" $ do
      let tokens = [loc (TokSymbol "("), loc (TokInt 2 BaseDec), loc (TokSymbol "+"), loc (TokInt 3 BaseDec), loc (TokSymbol ")"), loc (TokSymbol "*"), loc (TokInt 4 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary _ (Located _ (ExprParen _)) _ -> True `shouldBe` True
        _ -> fail "Expected correct structure with parentheses"

    it "parses comparison operators" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol "=="), loc (TokInt 5 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses logical AND" $ do
      let tokens = [loc (TokBool True), loc (TokSymbol "&&"), loc (TokBool False)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses logical OR" $ do
      let tokens = [loc (TokBool True), loc (TokSymbol "||"), loc (TokBool False)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

  describe "Parser.Expr - More Complex Expressions" $ do
    it "parses bitwise AND" $ do
      let tokens = [loc (TokInt 5 BaseDec), loc (TokSymbol "&"), loc (TokInt 3 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses bitwise OR" $ do
      let tokens = [loc (TokInt 5 BaseDec), loc (TokSymbol "|"), loc (TokInt 3 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses bitwise XOR" $ do
      let tokens = [loc (TokInt 5 BaseDec), loc (TokSymbol "^"), loc (TokInt 3 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses left shift" $ do
      let tokens = [loc (TokInt 1 BaseDec), loc (TokSymbol "<<"), loc (TokInt 3 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses right shift" $ do
      let tokens = [loc (TokInt 8 BaseDec), loc (TokSymbol ">>"), loc (TokInt 2 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses modulo" $ do
      let tokens = [loc (TokInt 10 BaseDec), loc (TokSymbol "%"), loc (TokInt 3 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses subtraction" $ do
      let tokens = [loc (TokInt 10 BaseDec), loc (TokSymbol "-"), loc (TokInt 3 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses division" $ do
      let tokens = [loc (TokInt 10 BaseDec), loc (TokSymbol "/"), loc (TokInt 2 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses not equals" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol "!="), loc (TokInt 5 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses less than" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol "<"), loc (TokInt 5 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses less than or equal" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol "<="), loc (TokInt 5 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses greater than" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol ">"), loc (TokInt 5 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses greater than or equal" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol ">="), loc (TokInt 5 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprBinary {} -> True `shouldBe` True
        _ -> fail "Expected ExprBinary"

    it "parses bitwise NOT" $ do
      let tokens = [loc (TokSymbol "~"), loc (TokInt 5 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprUnary {} -> True `shouldBe` True
        _ -> fail "Expected ExprUnary"

    it "parses unary plus" $ do
      let tokens = [loc (TokSymbol "+"), loc (TokInt 5 BaseDec)]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprUnary {} -> True `shouldBe` True
        _ -> fail "Expected ExprUnary"

    it "parses nested parentheses" $ do
      let tokens = [loc (TokSymbol "("), loc (TokSymbol "("), loc (TokInt 5 BaseDec), loc (TokSymbol ")"), loc (TokSymbol ")")]
      let result = runExprParser parseExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ExprParen (Located _ (ExprParen _)) -> True `shouldBe` True
        _ -> fail "Expected nested ExprParen"
