{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecLValue (lvalueSpec) where

import AST.Types.AST (LValue (..))
import AST.Types.Common (Located (..), VarName (..), unLocated)
import AST.Types.Literal (IntBase (..))
import Error (ParseError)
import Parser.LValue
import Parser.Utils (TokenParser, voidSpann)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runLValueParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runLValueParser parser = runParser parser "<test>"

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

lvalueSpec :: Spec
lvalueSpec = do
  describe "Parser.LValue - parseLVarRef" $ do
    it "parses a simple variable reference" $ do
      let tokens = [loc (TokIdentifier "x")]
      let result = runLValueParser parseLVarRef tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LVarRef (Located _ (VarName "x")) -> True `shouldBe` True
        _ -> fail "Expected LVarRef x"

    it "parses another variable reference" $ do
      let tokens = [loc (TokIdentifier "myVar")]
      let result = runLValueParser parseLVarRef tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LVarRef (Located _ (VarName "myVar")) -> True `shouldBe` True
        _ -> fail "Expected LVarRef myVar"

  describe "Parser.LValue - parseLValue" $ do
    it "parses a simple variable" $ do
      let tokens = [loc (TokIdentifier "x")]
      let result = runLValueParser parseLValue tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LVarRef _ -> True `shouldBe` True
        _ -> fail "Expected LVarRef"

    it "parses array indexing" $ do
      let tokens = [loc (TokIdentifier "arr"), loc (TokSymbol "["), loc (TokInt 0 BaseDec), loc (TokSymbol "]")]
      let result = runLValueParser parseLValue tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LArrayIndex _ _ -> True `shouldBe` True
        _ -> fail "Expected LArrayIndex"

    it "parses nested array indexing" $ do
      let tokens =
            [ loc (TokIdentifier "matrix"),
              loc (TokSymbol "["),
              loc (TokInt 0 BaseDec),
              loc (TokSymbol "]"),
              loc (TokSymbol "["),
              loc (TokInt 1 BaseDec),
              loc (TokSymbol "]")
            ]
      let result = runLValueParser parseLValue tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        LArrayIndex (Located _ (LArrayIndex _ _)) _ -> True `shouldBe` True
        _ -> fail "Expected nested LArrayIndex"
