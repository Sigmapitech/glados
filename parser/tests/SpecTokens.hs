{-# LANGUAGE OverloadedStrings #-}

module SpecTokens (tokensSpec) where

import AST.Types.Literal (IntBase (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Tokens (TokenContent (..))

tokensSpec :: Spec
tokensSpec = do
  describe "Tokens - Token data type" $ do
    describe "Token equality" $ do
      it "compares TokKeyword tokens" $ do
        TokKeyword "if" `shouldBe` TokKeyword "if"
        (TokKeyword "if" == TokKeyword "else") `shouldBe` False

      it "compares TokIdentifier tokens" $ do
        TokIdentifier "foo" `shouldBe` TokIdentifier "foo"
        (TokIdentifier "foo" == TokIdentifier "bar") `shouldBe` False

      it "compares TokSymbol tokens" $ do
        TokSymbol "+" `shouldBe` TokSymbol "+"
        (TokSymbol "+" == TokSymbol "-") `shouldBe` False

      it "compares TokInt tokens" $ do
        TokInt 42 BaseDec `shouldBe` TokInt 42 BaseDec
        (TokInt 42 BaseDec == TokInt 100 BaseDec) `shouldBe` False

      it "compares TokBool tokens" $ do
        TokBool True `shouldBe` TokBool True
        TokBool False `shouldBe` TokBool False
        (TokBool True == TokBool False) `shouldBe` False

      it "compares TokString tokens" $ do
        TokString "hello" `shouldBe` TokString "hello"
        (TokString "hello" == TokString "world") `shouldBe` False

      it "compares TokChar tokens" $ do
        TokChar 'a' `shouldBe` TokChar 'a'
        (TokChar 'a' == TokChar 'b') `shouldBe` False

      it "compares TokEOF tokens" $ do
        TokEOF `shouldBe` TokEOF

      it "different token types are not equal" $ do
        (TokInt 42 BaseDec == TokString "42") `shouldBe` False
        (TokBool True == TokKeyword "true") `shouldBe` False
        (TokIdentifier "x" == TokSymbol "x") `shouldBe` False

    describe "Token show instance" $ do
      it "shows TokKeyword" $ do
        show (TokKeyword "if") `shouldBe` "TokKeyword \"if\""

      it "shows TokIdentifier" $ do
        show (TokIdentifier "myVar") `shouldBe` "TokIdentifier \"myVar\""

      it "shows TokSymbol" $ do
        show (TokSymbol "+") `shouldBe` "TokSymbol \"+\""

      it "shows TokInt" $ do
        show (TokInt 42 BaseDec) `shouldBe` "TokInt 42"
        show (TokInt (-10) BaseDec) `shouldBe` "TokInt -10"

      it "shows TokBool" $ do
        show (TokBool True) `shouldBe` "TokBool True"
        show (TokBool False) `shouldBe` "TokBool False"

      it "shows TokString" $ do
        show (TokString "hello") `shouldBe` "TokString \"hello\""

      it "shows TokChar" $ do
        show (TokChar 'a') `shouldBe` "TokChar 'a'"

      it "shows TokEOF" $ do
        show TokEOF `shouldBe` "TokEOF"

    describe "Token in collections" $ do
      it "can be used in lists" $ do
        let tokens = [TokInt 1 BaseDec, TokSymbol "+", TokInt 2 BaseDec]
        length tokens `shouldBe` 3
        case tokens of
          (x : _) -> x `shouldBe` TokInt 1 BaseDec
          _ -> error "Should not be possible"

      it "can be filtered" $ do
        let isInt (TokInt _ _) = True
            isInt _ = False
            tokens = [TokInt 1 BaseDec, TokSymbol "+", TokInt 2 BaseDec, TokSymbol "*", TokInt 3 BaseDec]
            ints = filter isInt tokens
        ints `shouldBe` [TokInt 1 BaseDec, TokInt 2 BaseDec, TokInt 3 BaseDec]

      it "can be mapped" $ do
        let doubleInt (TokInt n b) = TokInt (n * 2) b
            doubleInt t = t
            tokens = [TokInt 1 BaseDec, TokInt 2 BaseDec, TokInt 3 BaseDec]
            doubled = map doubleInt tokens
        doubled `shouldBe` [TokInt 2 BaseDec, TokInt 4 BaseDec, TokInt 6 BaseDec]

  describe "Tokens - Token construction" $ do
    it "constructs keyword tokens" $ do
      let token = TokKeyword "return"
      token `shouldBe` TokKeyword "return"

    it "constructs identifier tokens" $ do
      let token = TokIdentifier "variableName"
      token `shouldBe` TokIdentifier "variableName"

    it "constructs symbol tokens" $ do
      let token = TokSymbol "=="
      token `shouldBe` TokSymbol "=="

    it "constructs integer tokens with positive values" $ do
      let token = TokInt 100 BaseDec
      token `shouldBe` TokInt 100 BaseDec

    it "constructs integer tokens with negative values" $ do
      let token = TokInt (-50) BaseDec
      token `shouldBe` TokInt (-50) BaseDec

    it "constructs integer tokens with zero" $ do
      let token = TokInt 0 BaseDec
      token `shouldBe` TokInt 0 BaseDec

    it "constructs boolean tokens" $ do
      TokBool True `shouldBe` TokBool True
      TokBool False `shouldBe` TokBool False

    it "constructs string tokens" $ do
      let token = TokString "Hello, World!"
      token `shouldBe` TokString "Hello, World!"

    it "constructs char tokens" $ do
      let token = TokChar 'z'
      token `shouldBe` TokChar 'z'

    it "constructs EOF token" $ do
      TokEOF `shouldBe` TokEOF

  describe "Tokens - showIntWithBase" $ do
    describe "Base decimal (BaseDec)" $ do
      it "shows positive decimal integers" $ do
        show (TokInt 42 BaseDec) `shouldBe` "TokInt 42"
        show (TokInt 100 BaseDec) `shouldBe` "TokInt 100"
        show (TokInt 999 BaseDec) `shouldBe` "TokInt 999"

      it "shows zero in decimal" $ do
        show (TokInt 0 BaseDec) `shouldBe` "TokInt 0"

      it "shows negative decimal integers" $ do
        show (TokInt (-42) BaseDec) `shouldBe` "TokInt -42"
        show (TokInt (-100) BaseDec) `shouldBe` "TokInt -100"

    describe "Base hexadecimal (BaseHex)" $ do
      it "shows positive hexadecimal integers" $ do
        show (TokInt 255 BaseHex) `shouldBe` "TokInt 0xff"
        show (TokInt 16 BaseHex) `shouldBe` "TokInt 0x10"
        show (TokInt 4095 BaseHex) `shouldBe` "TokInt 0xfff"

      it "shows zero in hexadecimal" $ do
        show (TokInt 0 BaseHex) `shouldBe` "TokInt 0x0"

      it "shows positive hexadecimal integers (otherwise branch)" $ do
        show (TokInt 1 BaseHex) `shouldBe` "TokInt 0x1"
        show (TokInt 42 BaseHex) `shouldBe` "TokInt 0x2a"

      it "shows negative hexadecimal integers" $ do
        show (TokInt (-255) BaseHex) `shouldBe` "TokInt -0xff"
        show (TokInt (-16) BaseHex) `shouldBe` "TokInt -0x10"

      it "shows small hexadecimal values" $ do
        show (TokInt 10 BaseHex) `shouldBe` "TokInt 0xa"
        show (TokInt 15 BaseHex) `shouldBe` "TokInt 0xf"

    describe "Base octal (BaseOct)" $ do
      it "shows positive octal integers" $ do
        show (TokInt 8 BaseOct) `shouldBe` "TokInt 0o10"
        show (TokInt 64 BaseOct) `shouldBe` "TokInt 0o100"
        show (TokInt 511 BaseOct) `shouldBe` "TokInt 0o777"

      it "shows zero in octal" $ do
        show (TokInt 0 BaseOct) `shouldBe` "TokInt 0o0"

      it "shows positive octal integers (otherwise branch)" $ do
        show (TokInt 1 BaseOct) `shouldBe` "TokInt 0o1"
        show (TokInt 42 BaseOct) `shouldBe` "TokInt 0o52"

      it "shows negative octal integers" $ do
        show (TokInt (-8) BaseOct) `shouldBe` "TokInt -0o10"
        show (TokInt (-64) BaseOct) `shouldBe` "TokInt -0o100"

      it "shows small octal values" $ do
        show (TokInt 7 BaseOct) `shouldBe` "TokInt 0o7"
        show (TokInt 1 BaseOct) `shouldBe` "TokInt 0o1"

    describe "Base binary (BaseBin)" $ do
      it "shows positive binary integers" $ do
        show (TokInt 5 BaseBin) `shouldBe` "TokInt 0b101"
        show (TokInt 8 BaseBin) `shouldBe` "TokInt 0b1000"
        show (TokInt 15 BaseBin) `shouldBe` "TokInt 0b1111"

      it "shows zero in binary" $ do
        show (TokInt 0 BaseBin) `shouldBe` "TokInt 0b0"

      it "shows positive binary integers (otherwise branch)" $ do
        show (TokInt 1 BaseBin) `shouldBe` "TokInt 0b1"
        show (TokInt 3 BaseBin) `shouldBe` "TokInt 0b11"

      it "shows negative binary integers" $ do
        show (TokInt (-5) BaseBin) `shouldBe` "TokInt -0b101"
        show (TokInt (-8) BaseBin) `shouldBe` "TokInt -0b1000"

      it "shows powers of two in binary" $ do
        show (TokInt 1 BaseBin) `shouldBe` "TokInt 0b1"
        show (TokInt 2 BaseBin) `shouldBe` "TokInt 0b10"
        show (TokInt 4 BaseBin) `shouldBe` "TokInt 0b100"
        show (TokInt 16 BaseBin) `shouldBe` "TokInt 0b10000"

      it "shows mixed bit patterns in binary" $ do
        show (TokInt 7 BaseBin) `shouldBe` "TokInt 0b111"
        show (TokInt 10 BaseBin) `shouldBe` "TokInt 0b1010"
        show (TokInt 255 BaseBin) `shouldBe` "TokInt 0b11111111"
