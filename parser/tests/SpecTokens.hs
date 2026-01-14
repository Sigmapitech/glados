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
