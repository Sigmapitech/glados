{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecDecl (declSpec) where

import AST.Types.AST (Decl (..), FunctionDecl (..), Visibility (..))
import AST.Types.Common (Located (..), unLocated)
import AST.Types.Literal (IntBase (..))
import Error (ParseError)
import Parser.Decl (parseDeclFunction, parseVisibility)
import Parser.Utils (TokenParser, voidSpann)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runDeclParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runDeclParser parser = runParser parser "<test>"

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

declSpec :: Spec
declSpec = do
  describe "Parser.Decl - parseVisibility" $ do
    it "parses static visibility" $ do
      let tokens = [loc (TokKeyword "static")]
      let result = runDeclParser parseVisibility tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` Static

    it "defaults to public when no static keyword" $ do
      let tokens = [loc (TokKeyword "fn")]
      let result = runDeclParser parseVisibility tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` Public

  describe "Parser.Decl - parseDeclFunction" $ do
    it "parses public function with no parameters" $ do
      let tokens = [loc (TokKeyword "fn"), loc (TokIdentifier "foo"), loc (TokSymbol "("), loc (TokSymbol ")"), loc (TokSymbol "->"), loc (TokKeyword "void"), loc (TokSymbol "{"), loc (TokSymbol "}")]
      let result = runDeclParser parseDeclFunction tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        DeclFunction Public _ -> True `shouldBe` True
        _ -> fail "Expected public DeclFunction"

    it "parses static function with no parameters" $ do
      let tokens = [loc (TokKeyword "static"), loc (TokKeyword "fn"), loc (TokIdentifier "foo"), loc (TokSymbol "("), loc (TokSymbol ")"), loc (TokSymbol "->"), loc (TokKeyword "void"), loc (TokSymbol "{"), loc (TokSymbol "}")]
      let result = runDeclParser parseDeclFunction tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        DeclFunction Static _ -> True `shouldBe` True
        _ -> fail "Expected static DeclFunction"

    it "parses function with parameters" $ do
      let tokens = [loc (TokKeyword "fn"), loc (TokIdentifier "add"), loc (TokSymbol "("), loc (TokIdentifier "a"), loc (TokSymbol ":"), loc (TokKeyword "int"), loc (TokSymbol ","), loc (TokIdentifier "b"), loc (TokSymbol ":"), loc (TokKeyword "int"), loc (TokSymbol ")"), loc (TokSymbol "->"), loc (TokKeyword "int"), loc (TokSymbol "{"), loc (TokKeyword "return"), loc (TokIdentifier "a"), loc (TokSymbol "+"), loc (TokIdentifier "b"), loc (TokSymbol "}")]
      let result = runDeclParser parseDeclFunction tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        DeclFunction _ funcDecl -> length (funcDeclParams funcDecl) `shouldBe` 2
        _ -> fail "Expected DeclFunction with 2 parameters"

    it "parses function returning int" $ do
      let tokens = [loc (TokKeyword "fn"), loc (TokIdentifier "getNum"), loc (TokSymbol "("), loc (TokSymbol ")"), loc (TokSymbol "->"), loc (TokKeyword "int"), loc (TokSymbol "{"), loc (TokKeyword "return"), loc (TokInt 42 BaseDec), loc (TokSymbol "}")]
      let result = runDeclParser parseDeclFunction tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        DeclFunction _ _ -> True `shouldBe` True
        _ -> fail "Expected DeclFunction"
