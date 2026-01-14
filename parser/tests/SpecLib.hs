{-# LANGUAGE OverloadedStrings #-}

module SpecLib (libSpec) where

import AST.Types.Common (unLocated)
import AST.Types.Literal (IntBase (..))
import Lib (lexFile, lexString)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Tokens (Token, TokenContent (..))
import Prelude hiding (getContents)

-- Helper to extract token contents from Located tokens
getContents :: [Token] -> [TokenContent]
getContents = map unLocated

-- Helper to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Helper to check if result is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- Helper to extract Right value
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight called on Left"

libSpec :: Spec
libSpec = do
  describe "Lib - lexString" $ do
    it "lexes a simple integer" $ do
      let result = lexString "42"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec]

    it "lexes keywords" $ do
      let result = lexString "int return"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "int", TokKeyword "return"]

    it "lexes identifiers" $ do
      let result = lexString "foo bar"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "foo", TokIdentifier "bar"]

    it "lexes symbols" $ do
      let result = lexString "+ == ->"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "+", TokSymbol "==", TokSymbol "->"]

    it "lexes strings" $ do
      let result = lexString "\"hello\""
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokString "hello"]

    it "handles comments" $ do
      let result = lexString "42 // comment\n99"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec, TokInt 99 BaseDec]

    it "handles block comments" $ do
      let result = lexString "42 /* comment */ 99"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec, TokInt 99 BaseDec]

    it "handles whitespace" $ do
      let result = lexString "  42  "
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec]

    it "lexes complex expression" $ do
      let result = lexString "int x = 42;"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "int", TokIdentifier "x", TokSymbol "=", TokInt 42 BaseDec, TokSymbol ";"]

    it "returns error for invalid character" $ do
      let result = lexString "@"
      result `shouldSatisfy` isLeft

    it "returns error for unclosed string" $ do
      let result = lexString "\"unclosed"
      result `shouldSatisfy` isLeft

    it "returns error for unclosed block comment" $ do
      let result = lexString "/* unclosed"
      result `shouldSatisfy` isLeft

    it "returns styled error message" $ do
      let result = lexString "@"
      case result of
        Left msg -> do
          msg `shouldSatisfy` (not . null)
          -- Error message should contain escape codes for styling
          msg `shouldSatisfy` (\s -> length s > 1)
        Right _ -> error "Expected Left but got Right"

  describe "Lib - lexFile" $ do
    it "lexes a valid C file" $ do
      result <- lexFile "../tests/valid_basic.c"
      result `shouldSatisfy` isRight
      -- Should contain some tokens
      case result of
        Right tokens -> length tokens `shouldSatisfy` (> 0)
        Left _ -> error "Expected Right but got Left"

    it "lexes file with operators" $ do
      result <- lexFile "../tests/valid_operators.c"
      result `shouldSatisfy` isRight

    it "returns error for file with invalid character" $ do
      result <- lexFile "../tests/error_char.c"
      result `shouldSatisfy` isLeft

    it "returns error for file with unclosed comment" $ do
      result <- lexFile "../tests/error_comment.c"
      result `shouldSatisfy` isLeft

    it "returns error for file with unclosed string" $ do
      result <- lexFile "../tests/error_string.c"
      result `shouldSatisfy` isLeft

    it "returns error for non-existent file" $ do
      result <- lexFile "../tests/nonexistent.c"
      result `shouldSatisfy` isLeft
      case result of
        Left msg -> msg `shouldSatisfy` (not . null)
        Right _ -> error "Expected Left but got Right"

    it "preserves file path in error messages" $ do
      result <- lexFile "../tests/error_char.c"
      case result of
        Left msg -> do
          msg `shouldSatisfy` (not . null)
          -- Should contain error information
          msg `shouldSatisfy` (\s -> length s > 10)
        Right _ -> error "Expected Left but got Right"

  describe "Lib - error message styling" $ do
    it "styles error output with colors" $ do
      let result = lexString "@invalid"
      case result of
        Left msg -> do
          -- Check that styling is present (contains escape codes)
          msg `shouldSatisfy` (\s -> "\ESC[" `isInfixOf` s)
        Right _ -> error "Expected error but got success"

    it "includes file path highlighting for file errors" $ do
      result <- lexFile "tests/error_char.c"
      case result of
        Left msg -> do
          -- Should be styled
          msg `shouldSatisfy` (not . null)
        Right _ -> error "Expected error but got success"

  describe "Lib - integration tests" $ do
    it "lexes empty input" $ do
      let result = lexString ""
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` []

    it "lexes whitespace only" $ do
      let result = lexString "   \n\t  "
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` []

    it "lexes comments only" $ do
      let result = lexString "// just a comment"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` []

    it "lexes mixed token types" $ do
      let result = lexString "int foo = 42 + bar;"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "int", TokIdentifier "foo", TokSymbol "=", TokInt 42 BaseDec, TokSymbol "+", TokIdentifier "bar", TokSymbol ";"]

    it "handles minus symbol" $ do
      let result = lexString "-123"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "-", TokInt 123 BaseDec]

    it "lexes function with parameters" $ do
      let result = lexString "int add(int a, int b) { return a + b; }"
      result `shouldSatisfy` isRight
      -- Should successfully tokenize
      case result of
        Right tokens -> length tokens `shouldSatisfy` (> 10)
        Left _ -> error "Expected Right but got Left"

-- Helper function for string search
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_ : xs') = xs : tails xs'
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
