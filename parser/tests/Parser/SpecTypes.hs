{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecTypes (typesSpec) where

import AST.Types.Common (Located (..), TypeName (..), unLocated)
import AST.Types.Literal (IntBase (..))
import AST.Types.Type
import Error (ParseError)
import Parser.Type
import Parser.Utils (TokenParser, voidSpann)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runTypeParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runTypeParser parser = runParser parser "<test>"

-- Helper to check if result is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- Helper to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Helper to extract Right value
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight called on Left"

-- Helper to create Located tokens
loc :: TokenContent -> Token
loc = Located voidSpann

typesSpec :: Spec
typesSpec = do
  describe "Parser.Type - parseSignedness" $ do
    it "parses signed" $ do
      let tokens = [loc (TokIdentifier "s")]
      let result = runTypeParser parseSignedness tokens
      result `shouldSatisfy` isRight
      fromRight result `shouldBe` Signed

    it "parses unsigned" $ do
      let tokens = [loc (TokIdentifier "u")]
      let result = runTypeParser parseSignedness tokens
      result `shouldSatisfy` isRight
      fromRight result `shouldBe` Unsigned

    it "fails on invalid signedness" $ do
      let tokens = [loc (TokIdentifier "x")]
      let result = runTypeParser parseSignedness tokens
      result `shouldSatisfy` isLeft

  describe "Parser.Type - parseIntType" $ do
    it "parses default int" $ do
      let tokens = [loc (TokKeyword "int")]
      let result = runTypeParser parseIntType tokens
      result `shouldSatisfy` isRight
      let Located _ intType = fromRight result
      intType `shouldBe` defaultIntType

    it "parses int<32>" $ do
      let tokens = [loc (TokKeyword "int"), loc (TokSymbol "<"), loc (TokInt 32 BaseDec), loc (TokSymbol ">")]
      let result = runTypeParser parseIntType tokens
      result `shouldSatisfy` isRight
      let Located _ (IntType (IntSize size) sign) = fromRight result
      size `shouldBe` 32
      sign `shouldBe` Signed

    it "parses int<64,u>" $ do
      let tokens = [loc (TokKeyword "int"), loc (TokSymbol "<"), loc (TokInt 64 BaseDec), loc (TokSymbol ","), loc (TokIdentifier "u"), loc (TokSymbol ">")]
      let result = runTypeParser parseIntType tokens
      result `shouldSatisfy` isRight
      let Located _ (IntType (IntSize size) sign) = fromRight result
      size `shouldBe` 64
      sign `shouldBe` Unsigned

    it "parses int<8,s>" $ do
      let tokens = [loc (TokKeyword "int"), loc (TokSymbol "<"), loc (TokInt 8 BaseDec), loc (TokSymbol ","), loc (TokIdentifier "s"), loc (TokSymbol ">")]
      let result = runTypeParser parseIntType tokens
      result `shouldSatisfy` isRight
      let Located _ (IntType (IntSize size) sign) = fromRight result
      size `shouldBe` 8
      sign `shouldBe` Signed

  describe "Parser.Type - parseFloatType" $ do
    it "parses default float" $ do
      let tokens = [loc (TokKeyword "float")]
      let result = runTypeParser parseFloatType tokens
      result `shouldSatisfy` isRight
      let Located _ floatType = fromRight result
      floatType `shouldBe` defaultFloatType

    it "parses float<32>" $ do
      let tokens = [loc (TokKeyword "float"), loc (TokSymbol "<"), loc (TokInt 32 BaseDec), loc (TokSymbol ">")]
      let result = runTypeParser parseFloatType tokens
      result `shouldSatisfy` isRight
      let Located _ (FloatType floatSize) = fromRight result
      floatSize `shouldBe` Float32

    it "parses float<64>" $ do
      let tokens = [loc (TokKeyword "float"), loc (TokSymbol "<"), loc (TokInt 64 BaseDec), loc (TokSymbol ">")]
      let result = runTypeParser parseFloatType tokens
      result `shouldSatisfy` isRight
      let Located _ (FloatType floatSize) = fromRight result
      floatSize `shouldBe` Float64

  describe "Parser.Type - parsePrimitiveType" $ do
    it "parses int" $ do
      let tokens = [loc (TokKeyword "int")]
      let result = runTypeParser parsePrimitiveType tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        PrimInt _ -> True `shouldBe` True
        _ -> fail "Expected PrimInt"

    it "parses float" $ do
      let tokens = [loc (TokKeyword "float")]
      let result = runTypeParser parsePrimitiveType tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        PrimFloat _ -> True `shouldBe` True
        _ -> fail "Expected PrimFloat"

    it "parses bool" $ do
      let tokens = [loc (TokKeyword "bool")]
      let result = runTypeParser parsePrimitiveType tokens
      result `shouldSatisfy` isRight
      let Located _ primType = fromRight result
      primType `shouldBe` PrimBool

    it "parses str" $ do
      let tokens = [loc (TokKeyword "str")]
      let result = runTypeParser parsePrimitiveType tokens
      result `shouldSatisfy` isRight
      let Located _ primType = fromRight result
      primType `shouldBe` PrimString

    it "parses void" $ do
      let tokens = [loc (TokKeyword "void")]
      let result = runTypeParser parsePrimitiveType tokens
      result `shouldSatisfy` isRight
      let Located _ primType = fromRight result
      primType `shouldBe` PrimNone

  describe "Parser.Type - parseArrayType" $ do
    it "parses [int]" $ do
      let tokens = [loc (TokSymbol "["), loc (TokKeyword "int"), loc (TokSymbol "]")]
      let result = runTypeParser parseArrayType tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ArrayType (QualifiedType _ (TypePrimitive (PrimInt _))) -> True `shouldBe` True
        _ -> fail "Expected ArrayType of int"

    it "parses [bool]" $ do
      let tokens = [loc (TokSymbol "["), loc (TokKeyword "bool"), loc (TokSymbol "]")]
      let result = runTypeParser parseArrayType tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ArrayType (QualifiedType _ (TypePrimitive PrimBool)) -> True `shouldBe` True
        _ -> fail "Expected ArrayType of bool"

    it "parses nested arrays [[int]]" $ do
      let tokens = [loc (TokSymbol "["), loc (TokSymbol "["), loc (TokKeyword "int"), loc (TokSymbol "]"), loc (TokSymbol "]")]
      let result = runTypeParser parseArrayType tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ArrayType (QualifiedType _ (TypeArray _)) -> True `shouldBe` True
        _ -> fail "Expected nested ArrayType"

  describe "Parser.Type - parseTypeNamed" $ do
    it "parses a type name" $ do
      let tokens = [loc (TokIdentifier "MyType")]
      let result = runTypeParser parseTypeNamed tokens
      result `shouldSatisfy` isRight
      let Located _ (TypeName name) = fromRight result
      name `shouldBe` "MyType"

    it "parses another type name" $ do
      let tokens = [loc (TokIdentifier "Coords")]
      let result = runTypeParser parseTypeNamed tokens
      result `shouldSatisfy` isRight
      let Located _ (TypeName name) = fromRight result
      name `shouldBe` "Coords"

  describe "Parser.Type - parseConstness" $ do
    it "parses const" $ do
      let tokens = [loc (TokKeyword "const")]
      let result = runTypeParser parseConstness tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` Const

    it "defaults to Mutable when const is absent" $ do
      let tokens = [loc (TokKeyword "int")]
      let result = runTypeParser parseConstness tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` Mutable

  describe "Parser.Type - parseQualifiedType" $ do
    it "parses const int" $ do
      let tokens = [loc (TokKeyword "const"), loc (TokKeyword "int")]
      let result = runTypeParser parseQualifiedType tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        QualifiedType Const (TypePrimitive (PrimInt _)) -> True `shouldBe` True
        _ -> fail "Expected const int"

    it "parses mutable int" $ do
      let tokens = [loc (TokKeyword "int")]
      let result = runTypeParser parseQualifiedType tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        QualifiedType Mutable (TypePrimitive (PrimInt _)) -> True `shouldBe` True
        _ -> fail "Expected mutable int"

    it "parses const [bool]" $ do
      let tokens = [loc (TokKeyword "const"), loc (TokSymbol "["), loc (TokKeyword "bool"), loc (TokSymbol "]")]
      let result = runTypeParser parseQualifiedType tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        QualifiedType Const (TypeArray _) -> True `shouldBe` True
        _ -> fail "Expected const array"
