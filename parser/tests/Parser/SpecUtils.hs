{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecUtils (utilsSpec) where

import AST.Types.Common (Located (..), unLocated)
import AST.Types.Literal (IntBase (..))
import Error (ParseError)
import Parser.Utils
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runUtilParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runUtilParser parser = runParser parser "<test>"

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

utilsSpec :: Spec
utilsSpec = do
  describe "Parser.Utils - matchKeyword" $ do
    it "matches a keyword" $ do
      let tokens = [loc (TokKeyword "if")]
      let result = runUtilParser (matchKeyword "if") tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` "if"

    it "fails on wrong keyword" $ do
      let tokens = [loc (TokKeyword "else")]
      let result = runUtilParser (matchKeyword "if") tokens
      result `shouldSatisfy` isLeft

    it "fails on non-keyword token" $ do
      let tokens = [loc (TokIdentifier "if")]
      let result = runUtilParser (matchKeyword "if") tokens
      result `shouldSatisfy` isLeft

    it "matches return keyword" $ do
      let tokens = [loc (TokKeyword "return")]
      let result = runUtilParser (matchKeyword "return") tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` "return"

    it "matches fn keyword" $ do
      let tokens = [loc (TokKeyword "fn")]
      let result = runUtilParser (matchKeyword "fn") tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` "fn"

  describe "Parser.Utils - matchSymbol" $ do
    it "matches a symbol" $ do
      let tokens = [loc (TokSymbol "+")]
      let result = runUtilParser (matchSymbol "+") tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` "+"

    it "fails on wrong symbol" $ do
      let tokens = [loc (TokSymbol "-")]
      let result = runUtilParser (matchSymbol "+") tokens
      result `shouldSatisfy` isLeft

    it "fails on non-symbol token" $ do
      let tokens = [loc (TokKeyword "+")]
      let result = runUtilParser (matchSymbol "+") tokens
      result `shouldSatisfy` isLeft

    it "matches (" $ do
      let tokens = [loc (TokSymbol "(")]
      let result = runUtilParser (matchSymbol "(") tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` "("

    it "matches {" $ do
      let tokens = [loc (TokSymbol "{")]
      let result = runUtilParser (matchSymbol "{") tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` "{"

    it "matches ;" $ do
      let tokens = [loc (TokSymbol ";")]
      let result = runUtilParser (matchSymbol ";") tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` ";"

  describe "Parser.Utils - token predicates" $ do
    it "isIdentifier recognizes identifiers" $ do
      isIdentifier (loc (TokIdentifier "foo")) `shouldBe` True
      isIdentifier (loc (TokKeyword "if")) `shouldBe` False
      isIdentifier (loc (TokInt 42 BaseDec)) `shouldBe` False

    it "isString recognizes strings" $ do
      isString (loc (TokString "hello")) `shouldBe` True
      isString (loc (TokIdentifier "hello")) `shouldBe` False
      isString (loc (TokInt 42 BaseDec)) `shouldBe` False

    it "isBool recognizes booleans" $ do
      isBool (loc (TokBool True)) `shouldBe` True
      isBool (loc (TokBool False)) `shouldBe` True
      isBool (loc (TokIdentifier "True")) `shouldBe` False
      isBool (loc (TokInt 1 BaseDec)) `shouldBe` False

    it "isSign recognizes sign symbols" $ do
      isSign (loc (TokSymbol "+")) `shouldBe` True
      isSign (loc (TokSymbol "-")) `shouldBe` True
      isSign (loc (TokSymbol "*")) `shouldBe` False
      isSign (loc (TokIdentifier "+")) `shouldBe` False

    it "isInt recognizes integers" $ do
      isInt (loc (TokInt 42 BaseDec)) `shouldBe` True
      isInt (loc (TokInt 0xFF BaseHex)) `shouldBe` True
      isInt (loc (TokIdentifier "42")) `shouldBe` False
      isInt (loc (TokString "42")) `shouldBe` False
