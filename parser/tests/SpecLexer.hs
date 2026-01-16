{-# LANGUAGE OverloadedStrings #-}

module SpecLexer (lexerSpec) where

import AST.Types.Common (unLocated)
import AST.Types.Literal (IntBase (..))
import Data.Text (Text)
import qualified Data.Text as T
import Error (GLaDOSError (..))
import Lexer (parseRawTokens)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))
import Prelude hiding (getContents)

-- Helper to run lexer
runLexer :: String -> Either (ParseErrorBundle Text GLaDOSError) [Token]
runLexer = runParser parseRawTokens "<test>" . T.pack

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

lexerSpec :: Spec
lexerSpec = do
  describe "Lexer - Integer tokens" $ do
    it "lexes a simple integer" $ do
      let result = runLexer "42"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec]

    it "lexes zero" $ do
      let result = runLexer "0"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 0 BaseDec]

    it "lexes multiple integers" $ do
      let result = runLexer "1 2 3"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 1 BaseDec, TokInt 2 BaseDec, TokInt 3 BaseDec]

    it "lexes large integers" $ do
      let result = runLexer "999999999"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 999999999 BaseDec]

  describe "Lexer - Keyword tokens" $ do
    it "lexes 'int' keyword" $ do
      let result = runLexer "int"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "int"]

    it "lexes 'bool' keyword" $ do
      let result = runLexer "bool"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "bool"]

    it "lexes 'void' keyword" $ do
      let result = runLexer "void"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "void"]

    it "lexes 'return' keyword" $ do
      let result = runLexer "return"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "return"]

    it "lexes 'if' keyword" $ do
      let result = runLexer "if"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "if"]

    it "lexes 'else' keyword" $ do
      let result = runLexer "else"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "else"]

    it "lexes 'while' keyword" $ do
      let result = runLexer "while"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "while"]

    it "lexes 'for' keyword" $ do
      let result = runLexer "for"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "for"]

    it "lexes 'True' keyword" $ do
      let result = runLexer "True"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokBool True]

    it "lexes 'False' keyword" $ do
      let result = runLexer "False"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokBool False]

    it "lexes multiple keywords" $ do
      let result = runLexer "if else return"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "if", TokKeyword "else", TokKeyword "return"]

  describe "Lexer - Identifier tokens" $ do
    it "lexes simple identifier" $ do
      let result = runLexer "foo"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "foo"]

    it "lexes identifier with underscores" $ do
      let result = runLexer "my_var"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "my_var"]

    it "lexes identifier starting with underscore" $ do
      let result = runLexer "_private"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "_private"]

    it "lexes identifier with numbers" $ do
      let result = runLexer "var123"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "var123"]

    it "lexes multiple identifiers" $ do
      let result = runLexer "foo bar baz"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "foo", TokIdentifier "bar", TokIdentifier "baz"]

    it "distinguishes identifiers from keywords" $ do
      let result = runLexer "inte myint"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "inte", TokIdentifier "myint"]

  describe "Lexer - Symbol tokens" $ do
    it "lexes single character symbols" $ do
      let result = runLexer "+ - * /"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "+", TokSymbol "-", TokSymbol "*", TokSymbol "/"]

    it "lexes comparison symbols" $ do
      let result = runLexer "== != <= >="
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "==", TokSymbol "!=", TokSymbol "<=", TokSymbol ">="]

    it "lexes logical operators" $ do
      let result = runLexer "&& ||"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "&&", TokSymbol "||"]

    it "lexes arrow symbols" $ do
      let result = runLexer "-> =>"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "->", TokSymbol "=", TokSymbol ">"]

    it "lexes parentheses" $ do
      let result = runLexer "( )"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "(", TokSymbol ")"]

    it "lexes braces" $ do
      let result = runLexer "{ }"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "{", TokSymbol "}"]

    it "lexes brackets" $ do
      let result = runLexer "[ ]"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol "[", TokSymbol "]"]

    it "lexes miscellaneous symbols" $ do
      let result = runLexer "; = ,"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokSymbol ";", TokSymbol "=", TokSymbol ","]

  describe "Lexer - String tokens" $ do
    it "lexes simple string" $ do
      let result = runLexer "\"hello\""
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokString "hello"]

    it "lexes empty string" $ do
      let result = runLexer "\"\""
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokString ""]

    it "lexes string with spaces" $ do
      let result = runLexer "\"hello world\""
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokString "hello world"]

    it "lexes string with escape sequences" $ do
      let result = runLexer "\"hello\\nworld\""
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokString "hello\nworld"]

    it "lexes multiple strings" $ do
      let result = runLexer "\"foo\" \"bar\""
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokString "foo", TokString "bar"]

    it "fails on unclosed string" $ do
      let result = runLexer "\"unclosed"
      result `shouldSatisfy` isLeft

    it "fails on string with newline" $ do
      let result = runLexer "\"hello\nworld\""
      result `shouldSatisfy` isLeft

    it "fail on string with EOF" $ do
      let result = runLexer "\""
      result `shouldSatisfy` isLeft

  describe "Lexer - Comment handling" $ do
    it "handles line comments with //" $ do
      let result = runLexer "42 // comment"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec]

    it "handles line comments with #" $ do
      let result = runLexer "42 # comment"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec]

    it "handles block comments" $ do
      let result = runLexer "42 /* comment */ 99"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec, TokInt 99 BaseDec]

    it "handles multi-line block comments" $ do
      let result = runLexer "42 /* line1\nline2 */ 99"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec, TokInt 99 BaseDec]

    it "fails on unclosed block comment" $ do
      let result = runLexer "42 /* unclosed"
      result `shouldSatisfy` isLeft
    it "fail on unclosed EOF comment" $ do
      let result = runLexer "/*"
      result `shouldSatisfy` isLeft

  describe "Lexer - Whitespace handling" $ do
    it "handles leading whitespace" $ do
      let result = runLexer "   42"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec]

    it "handles trailing whitespace" $ do
      let result = runLexer "42   "
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec]

    it "handles tabs" $ do
      let result = runLexer "\t42\t"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec]

    it "handles newlines" $ do
      let result = runLexer "42\n99"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokInt 42 BaseDec, TokInt 99 BaseDec]

  describe "Lexer - Error handling" $ do
    it "fails on invalid character @" $ do
      let result = runLexer "@"
      result `shouldSatisfy` isLeft

    it "fails on invalid character $" $ do
      let result = runLexer "$"
      result `shouldSatisfy` isLeft

    it "fails on backtick" $ do
      let result = runLexer "`"
      result `shouldSatisfy` isLeft

  describe "Lexer - Complex expressions" $ do
    it "lexes a simple expression" $ do
      let result = runLexer "x + 1"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "x", TokSymbol "+", TokInt 1 BaseDec]

    it "lexes function call" $ do
      let result = runLexer "foo(1, 2)"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokIdentifier "foo", TokSymbol "(", TokInt 1 BaseDec, TokSymbol ",", TokInt 2 BaseDec, TokSymbol ")"]

    it "lexes if statement structure" $ do
      let result = runLexer "if ( x == 5 ) { return 1 ; }"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "if", TokSymbol "(", TokIdentifier "x", TokSymbol "==", TokInt 5 BaseDec, TokSymbol ")", TokSymbol "{", TokKeyword "return", TokInt 1 BaseDec, TokSymbol ";", TokSymbol "}"]

    it "lexes variable declaration" $ do
      let result = runLexer "int x = 42;"
      result `shouldSatisfy` isRight
      getContents (fromRight result) `shouldBe` [TokKeyword "int", TokIdentifier "x", TokSymbol "=", TokInt 42 BaseDec, TokSymbol ";"]
