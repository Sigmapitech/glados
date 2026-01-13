{-# LANGUAGE OverloadedStrings #-}

module SpecTokens (tokensSpec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Tokens (TokenConent (..))

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
        TokInt 42 `shouldBe` TokInt 42
        (TokInt 42 == TokInt 100) `shouldBe` False

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
        (TokInt 42 == TokString "42") `shouldBe` False
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
        show (TokInt 42) `shouldBe` "TokInt 42"
        show (TokInt (-10)) `shouldBe` "TokInt (-10)"

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
        let tokens = [TokInt 1, TokSymbol "+", TokInt 2]
        length tokens `shouldBe` 3
        head tokens `shouldBe` TokInt 1

      it "can be filtered" $ do
        let isInt (TokInt _) = True
            isInt _ = False
            tokens = [TokInt 1, TokSymbol "+", TokInt 2, TokSymbol "*", TokInt 3]
            ints = filter isInt tokens
        ints `shouldBe` [TokInt 1, TokInt 2, TokInt 3]

      it "can be mapped" $ do
        let doubleInt (TokInt n) = TokInt (n * 2)
            doubleInt t = t
            tokens = [TokInt 1, TokInt 2, TokInt 3]
            doubled = map doubleInt tokens
        doubled `shouldBe` [TokInt 2, TokInt 4, TokInt 6]

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
      let token = TokInt 100
      token `shouldBe` TokInt 100

    it "constructs integer tokens with negative values" $ do
      let token = TokInt (-50)
      token `shouldBe` TokInt (-50)

    it "constructs integer tokens with zero" $ do
      let token = TokInt 0
      token `shouldBe` TokInt 0

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
