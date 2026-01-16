{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecStmt (stmtSpec) where

import AST.Types.AST (Block (..), Stmt (..))
import AST.Types.Common (Located (..), unLocated)
import AST.Types.Literal (IntBase (..))
import Error (ParseError)
import Parser.Stmt
  ( parseBlock,
    parseStmt,
    parseStmtAssign,
    parseStmtBreak,
    parseStmtContinue,
    parseStmtExpr,
    parseStmtReturn,
    parseStmtVarDecl,
  )
import Parser.Utils (TokenParser, voidSpann)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runStmtParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runStmtParser parser = runParser parser "<test>"

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

stmtSpec :: Spec
stmtSpec = do
  describe "Parser.Stmt - parseStmtBreak" $ do
    it "parses break statement" $ do
      let tokens = [loc (TokKeyword "break")]
      let result = runStmtParser parseStmtBreak tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtBreak -> True `shouldBe` True
        _ -> fail "Expected StmtBreak"

  describe "Parser.Stmt - parseStmtContinue" $ do
    it "parses continue statement" $ do
      let tokens = [loc (TokKeyword "continue")]
      let result = runStmtParser parseStmtContinue tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtContinue -> True `shouldBe` True
        _ -> fail "Expected StmtContinue"

  describe "Parser.Stmt - parseStmtReturn" $ do
    it "parses return without value" $ do
      let tokens = [loc (TokKeyword "return")]
      let result = runStmtParser parseStmtReturn tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtReturn Nothing -> True `shouldBe` True
        _ -> fail "Expected StmtReturn Nothing"

    it "parses return with value" $ do
      let tokens = [loc (TokKeyword "return"), loc (TokInt 42 BaseDec)]
      let result = runStmtParser parseStmtReturn tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtReturn (Just _) -> True `shouldBe` True
        _ -> fail "Expected StmtReturn with value"

  describe "Parser.Stmt - parseStmtExpr" $ do
    it "parses expression statement" $ do
      let tokens = [loc (TokInt 42 BaseDec)]
      let result = runStmtParser parseStmtExpr tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtExpr _ -> True `shouldBe` True
        _ -> fail "Expected StmtExpr"

  describe "Parser.Stmt - parseStmtVarDecl" $ do
    it "parses variable declaration without initialization" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol ":"), loc (TokKeyword "int")]
      let result = runStmtParser parseStmtVarDecl tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtVarDecl _ _ Nothing -> True `shouldBe` True
        _ -> fail "Expected StmtVarDecl without initializer"

    it "parses variable declaration with initialization" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol ":"), loc (TokKeyword "int"), loc (TokSymbol "="), loc (TokInt 42 BaseDec)]
      let result = runStmtParser parseStmtVarDecl tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtVarDecl _ _ (Just _) -> True `shouldBe` True
        _ -> fail "Expected StmtVarDecl with initializer"

  describe "Parser.Stmt - parseStmtAssign" $ do
    it "parses simple assignment" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol "="), loc (TokInt 42 BaseDec)]
      let result = runStmtParser parseStmtAssign tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtAssign {} -> True `shouldBe` True
        _ -> fail "Expected StmtAssign"

    it "parses compound assignment" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol "+="), loc (TokInt 10 BaseDec)]
      let result = runStmtParser parseStmtAssign tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtAssign {} -> True `shouldBe` True
        _ -> fail "Expected StmtAssign"

  describe "Parser.Stmt - parseBlock" $ do
    it "parses empty block" $ do
      let tokens = [loc (TokSymbol "{"), loc (TokSymbol "}")]
      let result = runStmtParser parseBlock tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        Block _ stmts -> length stmts `shouldBe` 0

    it "parses block with one statement" $ do
      let tokens = [loc (TokSymbol "{"), loc (TokKeyword "break"), loc (TokSymbol ";"), loc (TokSymbol "}")]
      let result = runStmtParser parseBlock tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        Block _ stmts -> length stmts `shouldBe` 1

    it "parses block with multiple statements" $ do
      let tokens =
            [ loc (TokSymbol "{"),
              loc (TokKeyword "break"),
              loc (TokSymbol ";"),
              loc (TokKeyword "continue"),
              loc (TokSymbol ";"),
              loc (TokSymbol "}")
            ]
      let result = runStmtParser parseBlock tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        Block _ stmts -> length stmts `shouldBe` 2

  describe "Parser.Stmt - parseStmt" $ do
    it "parses various statement types" $ do
      let tokens = [loc (TokKeyword "break")]
      let result = runStmtParser parseStmt tokens
      result `shouldSatisfy` isRight

    it "tries variable declaration first" $ do
      let tokens = [loc (TokIdentifier "x"), loc (TokSymbol ":"), loc (TokKeyword "int")]
      let result = runStmtParser parseStmt tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        StmtVarDecl {} -> True `shouldBe` True
        _ -> fail "Expected StmtVarDecl"
