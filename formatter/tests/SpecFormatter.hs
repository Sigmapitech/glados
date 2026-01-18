{-# LANGUAGE OverloadedStrings #-}

module SpecFormatter (spec) where

import AST.Types
import AST.Types.Common (Located (..), SourceSpan, initialPos, spanSingle)
import Data.Text (Text)
import Formatter
import Test.Hspec

-- Helper for tests
dummySpan :: SourceSpan
dummySpan = spanSingle (initialPos "<test>")

spec :: Spec
spec = do
  describe "Formatter" $ do
    describe "formatVisibility" $ do
      it "formats public visibility" $ do
        formatVisibility Public `shouldBe` "pub"

      it "formats static visibility" $ do
        formatVisibility Static `shouldBe` "static"

    describe "formatType" $ do
      it "formats primitive int type" $ do
        formatType (TypePrimitive (PrimInt defaultIntType)) `shouldBe` "int"

      it "formats primitive bool type" $ do
        formatType (TypePrimitive PrimBool) `shouldBe` "bool"

      it "formats primitive string type" $ do
        formatType (TypePrimitive PrimString) `shouldBe` "str"

      it "formats array type" $ do
        let qualInt = QualifiedType Mutable (TypePrimitive (PrimInt defaultIntType))
        let arrayType = TypeArray (ArrayType qualInt)
        formatType arrayType `shouldBe` "[int]"

      it "formats nested array type" $ do
        let qualInt = QualifiedType Mutable (TypePrimitive (PrimInt defaultIntType))
        let innerArray = TypeArray (ArrayType qualInt)
        let qualInnerArray = QualifiedType Mutable innerArray
        let outerArray = TypeArray (ArrayType qualInnerArray)
        formatType outerArray `shouldBe` "[[int]]"

      it "formats named type" $ do
        formatType (TypeNamed (TypeName "MyType")) `shouldBe` "MyType"

    describe "formatQualifiedType" $ do
      it "formats mutable type" $ do
        let qtype = QualifiedType Mutable (TypePrimitive (PrimInt defaultIntType))
        formatQualifiedType qtype `shouldBe` "int"

      it "formats const type" $ do
        let qtype = QualifiedType Const (TypePrimitive (PrimInt defaultIntType))
        formatQualifiedType qtype `shouldBe` "const int"

    describe "formatBinaryOp" $ do
      it "formats addition operator" $ do
        formatBinaryOp OpAdd `shouldBe` "+"

      it "formats equality operator" $ do
        formatBinaryOp OpEq `shouldBe` "=="

      it "formats logical and operator" $ do
        formatBinaryOp OpAnd `shouldBe` "&&"

      it "formats shift left operator" $ do
        formatBinaryOp OpShl `shouldBe` "<<"

    describe "formatUnaryOp" $ do
      it "formats negation operator" $ do
        formatUnaryOp OpNeg `shouldBe` "-"

      it "formats logical not operator" $ do
        formatUnaryOp OpNot `shouldBe` "!"

    describe "formatAssignOp" $ do
      it "formats simple assignment" $ do
        formatAssignOp AssignSimple `shouldBe` "="

      it "formats add assignment" $ do
        formatAssignOp AssignAdd `shouldBe` "+="

      it "formats shift left assignment" $ do
        formatAssignOp AssignShl `shouldBe` "<<="

    describe "precedence" $ do
      it "multiplication has higher precedence than addition" $ do
        precedence OpMul `shouldSatisfy` (> precedence OpAdd)

      it "addition has higher precedence than comparison" $ do
        precedence OpAdd `shouldSatisfy` (> precedence OpEq)

      it "comparison has higher precedence than logical and" $ do
        precedence OpEq `shouldSatisfy` (> precedence OpAnd)

    describe "indentation" $ do
      it "generates correct indentation with default options" $ do
        let opts = defaultFormatOptions
        indentation opts 0 `shouldBe` ""
        indentation opts 1 `shouldBe` "    "
        indentation opts 2 `shouldBe` "        "

      it "respects custom indent size" $ do
        let opts = defaultFormatOptions {formatIndentSize = 2}
        indentation opts 1 `shouldBe` "  "
        indentation opts 2 `shouldBe` "    "

    describe "formatImportDecl" $ do
      it "formats import all" $ do
        let importDecl =
              ImportDecl
                (ModulePath [Located dummySpan (ModuleName "std")])
                ImportAll
        formatImportDecl importDecl `shouldBe` "import std;"

      it "formats import with names" $ do
        let importDecl =
              ImportDecl
                (ModulePath [Located dummySpan (ModuleName "std")])
                (ImportNames [Located dummySpan (VarName "print")])
        formatImportDecl importDecl `shouldBe` "from std import print;"

      it "formats import wildcard" $ do
        let importDecl =
              ImportDecl
                (ModulePath [Located dummySpan (ModuleName "std")])
                ImportWildcard
        formatImportDecl importDecl `shouldBe` "from std import *;"

      it "formats nested module path" $ do
        let importDecl =
              ImportDecl
                ( ModulePath
                    [ Located dummySpan (ModuleName "std"),
                      Located dummySpan (ModuleName "io")
                    ]
                )
                ImportAll
        formatImportDecl importDecl `shouldBe` "import std.io;"

    describe "defaultFormatOptions" $ do
      it "has sensible defaults" $ do
        formatIndentSize defaultFormatOptions `shouldBe` 4
        formatMaxLineLength defaultFormatOptions `shouldBe` 100
        formatUseSpaces defaultFormatOptions `shouldBe` True
        formatArrayWidth defaultFormatOptions `shouldBe` 80
        formatReorderImports defaultFormatOptions `shouldBe` False
        formatNormalizeComments defaultFormatOptions `shouldBe` False
