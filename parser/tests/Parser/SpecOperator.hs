{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecOperator (operatorSpec) where

import AST.Types.Common (Located (..), unLocated)
import AST.Types.Operator (AssignOp (..), BinaryOp (..), UnaryOp (..))
import Error (ParseError)
import Parser.Operator
import Parser.Utils (TokenParser, voidSpann)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runOpParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runOpParser parser = runParser parser "<test>"

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

operatorSpec :: Spec
operatorSpec = do
  describe "Parser.Operator - parseAssignOp" $ do
    it "parses =" $ do
      let tokens = [loc (TokSymbol "=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignSimple

    it "parses +=" $ do
      let tokens = [loc (TokSymbol "+=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignAdd

    it "parses -=" $ do
      let tokens = [loc (TokSymbol "-=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignSub

    it "parses *=" $ do
      let tokens = [loc (TokSymbol "*=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignMul

    it "parses /=" $ do
      let tokens = [loc (TokSymbol "/=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignDiv

    it "parses %=" $ do
      let tokens = [loc (TokSymbol "%=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignMod

    it "parses &=" $ do
      let tokens = [loc (TokSymbol "&=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignBitAnd

    it "parses |=" $ do
      let tokens = [loc (TokSymbol "|=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignBitOr

    it "parses ^=" $ do
      let tokens = [loc (TokSymbol "^=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignBitXor

    it "parses <<=" $ do
      let tokens = [loc (TokSymbol "<<=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignShl

    it "parses >>=" $ do
      let tokens = [loc (TokSymbol ">>=")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` AssignShr

    it "fails on invalid symbol" $ do
      let tokens = [loc (TokSymbol "===")]
      let result = runOpParser parseAssignOp tokens
      result `shouldSatisfy` isLeft

  describe "Parser.Operator - parseBinaryOp" $ do
    it "parses +" $ do
      let tokens = [loc (TokSymbol "+")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpAdd

    it "parses -" $ do
      let tokens = [loc (TokSymbol "-")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpSub

    it "parses *" $ do
      let tokens = [loc (TokSymbol "*")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpMul

    it "parses /" $ do
      let tokens = [loc (TokSymbol "/")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpDiv

    it "parses %" $ do
      let tokens = [loc (TokSymbol "%")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpMod

    it "parses ==" $ do
      let tokens = [loc (TokSymbol "==")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpEq

    it "parses !=" $ do
      let tokens = [loc (TokSymbol "!=")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpNeq

    it "parses <" $ do
      let tokens = [loc (TokSymbol "<")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpLt

    it "parses <=" $ do
      let tokens = [loc (TokSymbol "<=")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpLte

    it "parses >" $ do
      let tokens = [loc (TokSymbol ">")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpGt

    it "parses >=" $ do
      let tokens = [loc (TokSymbol ">=")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpGte

    it "parses &&" $ do
      let tokens = [loc (TokSymbol "&&")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpAnd

    it "parses ||" $ do
      let tokens = [loc (TokSymbol "||")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpOr

    it "parses &" $ do
      let tokens = [loc (TokSymbol "&")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpBitAnd

    it "parses |" $ do
      let tokens = [loc (TokSymbol "|")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpBitOr

    it "parses ^" $ do
      let tokens = [loc (TokSymbol "^")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpBitXor

    it "parses <<" $ do
      let tokens = [loc (TokSymbol "<<")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpShl

    it "parses >>" $ do
      let tokens = [loc (TokSymbol ">>")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpShr

    it "fails on invalid symbol" $ do
      let tokens = [loc (TokSymbol "**")]
      let result = runOpParser parseBinaryOp tokens
      result `shouldSatisfy` isLeft

  describe "Parser.Operator - parseUnaryOp" $ do
    it "parses -" $ do
      let tokens = [loc (TokSymbol "-")]
      let result = runOpParser parseUnaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpNeg

    it "parses !" $ do
      let tokens = [loc (TokSymbol "!")]
      let result = runOpParser parseUnaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpNot

    it "parses ~" $ do
      let tokens = [loc (TokSymbol "~")]
      let result = runOpParser parseUnaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpBitNot

    it "parses +" $ do
      let tokens = [loc (TokSymbol "+")]
      let result = runOpParser parseUnaryOp tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` OpPos

    it "fails on invalid symbol" $ do
      let tokens = [loc (TokSymbol "*")]
      let result = runOpParser parseUnaryOp tokens
      result `shouldSatisfy` isLeft
