{-# LANGUAGE OverloadedStrings #-}

module SpecAST (specAST) where

import AST.Types
import Test.Hspec

specAST :: Spec
specAST = do
  describe "AST.Types.Common" $ do
    it "creates initial source position correctly" $ do
      let pos = initialPos "test.quant"
      posLine pos `shouldBe` Line 1
      posColumn pos `shouldBe` Column 1
      posOffset pos `shouldBe` Offset 0

    it "advances column correctly" $ do
      let pos = initialPos "test.quant"
          pos' = advanceCol pos
      posColumn pos' `shouldBe` Column 2
      posOffset pos' `shouldBe` Offset 1

    it "advances line correctly" $ do
      let pos = initialPos "test.quant"
          pos' = advanceLine pos
      posLine pos' `shouldBe` Line 2
      posColumn pos' `shouldBe` Column 1

    it "displays position in file:line:col format" $ do
      let pos = initialPos "test.quant"
      displayPos pos `shouldBe` "test.quant:1:1"

  describe "AST.Types.Type" $ do
    it "default int type is 32-bit signed" $ do
      unIntSize (intTypeSize defaultIntType) `shouldBe` 32
      intTypeSign defaultIntType `shouldBe` Signed

    it "default float type is 64-bit" $ do
      floatTypeSize defaultFloatType `shouldBe` Float64

    it "isNumericType identifies numeric types" $ do
      let intType = TypePrimitive (PrimInt defaultIntType)
          floatType = TypePrimitive (PrimFloat defaultFloatType)
          boolType = TypePrimitive PrimBool
      isNumericType intType `shouldBe` True
      isNumericType floatType `shouldBe` True
      isNumericType boolType `shouldBe` False

  describe "AST.Types.Type Show instances" $ do
    it "displays default int as 'int'" $ do
      show defaultIntType `shouldBe` "int"

    it "displays int<8> correctly" $ do
      show (IntType int8 Signed) `shouldBe` "int<8>"

    it "displays int<16, u> correctly" $ do
      show (IntType int16 Unsigned) `shouldBe` "int<16, u>"

    it "displays default float as 'float'" $ do
      show defaultFloatType `shouldBe` "float"

    it "displays float<32> correctly" $ do
      show (FloatType Float32) `shouldBe` "float<32>"

    it "displays array types correctly" $ do
      let arrType = ArrayType (TypePrimitive (PrimInt defaultIntType))
      show arrType `shouldBe` "[int]"

    it "displays qualified const types correctly" $ do
      let qtype = QualifiedType Const (TypePrimitive (PrimInt defaultIntType))
      show qtype `shouldBe` "const int"

    it "displays result types correctly" $ do
      let resType = ResultType (TypePrimitive (PrimInt defaultIntType)) (ErrorName "DivisionError")
      show resType `shouldBe` "int | DivisionError"

    it "displays error types without fields" $ do
      let errType = ErrorType (ErrorName "SimpleError") []
      show errType `shouldBe` "error SimpleError"

    it "displays error types with fields" $ do
      let errType =
            ErrorType
              (ErrorName "DivisionError")
              [ ErrorField (FieldName "dividend") (TypePrimitive (PrimInt defaultIntType)),
                ErrorField (FieldName "divisor") (TypePrimitive (PrimInt defaultIntType))
              ]
      show errType `shouldBe` "error DivisionError { dividend: int, divisor: int }"

    it "displays error sets correctly" $ do
      let errSet =
            ErrorSet
              (ErrorName "MathError")
              [ ErrorMemberSingle (ErrorName "DivisionError"),
                ErrorMemberSingle (ErrorName "OverflowError")
              ]
      show errSet `shouldBe` "error MathError = DivisionError | OverflowError"

  describe "AST.Types.Operator" $ do
    it "multiplication has higher precedence than addition" $ do
      binaryOpPrecedence OpMul > binaryOpPrecedence OpAdd `shouldBe` True

    it "comparison operators are non-associative" $ do
      binaryOpAssoc OpLt `shouldBe` NonAssoc
      binaryOpAssoc OpEq `shouldBe` NonAssoc

    it "arithmetic operators are left-associative" $ do
      binaryOpAssoc OpAdd `shouldBe` LeftAssoc
      binaryOpAssoc OpSub `shouldBe` LeftAssoc
