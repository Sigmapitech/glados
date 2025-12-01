module SpecParser (parserSpec) where

import Ast (SExpr (..), SymbolName (..))
import Control.Exception (SomeException, try)
import Parser (parseFile, parseString)
import System.IO (stderr)
import System.IO.Silently (hSilence)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

parseStringWrapper :: String -> IO [SExpr]
parseStringWrapper = hSilence [stderr] . parseString

parseFileWrapper :: FilePath -> IO [SExpr]
parseFileWrapper = hSilence [stderr] . parseFile

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

parserSpec :: Spec
parserSpec = do
  describe "Parser - Atoms" $ do
    it "parses a single integer" $ do
      result <- parseStringWrapper "42"
      result `shouldBe` [SInteger 42]

    it "parses negative integers" $ do
      result <- parseStringWrapper "-123"
      result `shouldBe` [SInteger (-123)]

    it "parses zero" $ do
      result <- parseStringWrapper "0"
      result `shouldBe` [SInteger 0]

    it "parses boolean true" $ do
      result <- parseStringWrapper "#t"
      result `shouldBe` [SBool True]

    it "parses boolean false" $ do
      result <- parseStringWrapper "#f"
      result `shouldBe` [SBool False]

    it "parses a simple symbol" $ do
      result <- parseStringWrapper "foo"
      result `shouldBe` [SSymbol (SymbolName "foo")]

    it "parses symbols with special characters" $ do
      result <- parseStringWrapper "+"
      result `shouldBe` [SSymbol (SymbolName "+")]

    it "parses symbols with mixed special chars" $ do
      result <- parseStringWrapper ">=?"
      result `shouldBe` [SSymbol (SymbolName ">=?")]

  describe "Parser - Lists" $ do
    it "parses an empty list" $ do
      result <- parseStringWrapper "()"
      result `shouldBe` [SList []]

    it "parses a simple list" $ do
      result <- parseStringWrapper "(+ 1 2)"
      result `shouldBe` [SList [SSymbol (SymbolName "+"), SInteger 1, SInteger 2]]

    it "parses nested lists" $ do
      result <- parseStringWrapper "(+ (* 2 3) 4)"
      result `shouldBe` [SList [SSymbol (SymbolName "+"), SList [SSymbol (SymbolName "*"), SInteger 2, SInteger 3], SInteger 4]]

    it "parses deeply nested lists" $ do
      result <- parseStringWrapper "(((1)))"
      result `shouldBe` [SList [SList [SList [SInteger 1]]]]

    it "parses list with booleans" $ do
      result <- parseStringWrapper "(if #t 1 0)"
      result `shouldBe` [SList [SSymbol (SymbolName "if"), SBool True, SInteger 1, SInteger 0]]

  describe "Parser - Multiple expressions" $ do
    it "parses multiple atoms" $ do
      result <- parseStringWrapper "1 2 3"
      result `shouldBe` [SInteger 1, SInteger 2, SInteger 3]

    it "parses multiple lists" $ do
      result <- parseStringWrapper "(+ 1 2) (* 3 4)"
      result `shouldBe` [SList [SSymbol (SymbolName "+"), SInteger 1, SInteger 2], SList [SSymbol (SymbolName "*"), SInteger 3, SInteger 4]]

    it "parses mixed expressions" $ do
      result <- parseStringWrapper "42 (foo bar) baz"
      result `shouldBe` [SInteger 42, SList [SSymbol (SymbolName "foo"), SSymbol (SymbolName "bar")], SSymbol (SymbolName "baz")]

  describe "Parser - Whitespace and comments" $ do
    it "handles leading whitespace" $ do
      result <- parseStringWrapper "   42"
      result `shouldBe` [SInteger 42]

    it "handles trailing whitespace" $ do
      result <- parseStringWrapper "42   "
      result `shouldBe` [SInteger 42]

    it "handles whitespace between expressions" $ do
      result <- parseStringWrapper "1    2    3"
      result `shouldBe` [SInteger 1, SInteger 2, SInteger 3]

    it "handles newlines" $ do
      result <- parseStringWrapper "1\n2\n3"
      result `shouldBe` [SInteger 1, SInteger 2, SInteger 3]

    it "ignores line comments" $ do
      result <- parseStringWrapper "; this is a comment\n42"
      result `shouldBe` [SInteger 42]

    it "ignores comments at end of line" $ do
      result <- parseStringWrapper "42 ; comment here"
      result `shouldBe` [SInteger 42]

    it "handles multiple comments" $ do
      result <- parseStringWrapper "; comment 1\n42 ; comment 2\n; comment 3\n99"
      result `shouldBe` [SInteger 42, SInteger 99]

    it "handle simple block comments" $ do
      result <- parseStringWrapper "#| This is a block comment |# 123"
      result `shouldBe` [SInteger 123]

    it "handle block comments with newlines" $ do
      result <- parseStringWrapper "#| This is a\nmulti-line block comment |# 789"
      result `shouldBe` [SInteger 789]

    it "handle nested block comments" $ do
      result <- parseStringWrapper "#| Outer comment #| Inner comment |# End of outer |# 456"
      result `shouldBe` [SInteger 456]

    it "handles comments in block comments" $ do
      result <- parseStringWrapper "#| Comment with ; line comment inside\n |# 321"
      result `shouldBe` [SInteger 321]

  describe "Parser - Error handling" $ do
    it "fails on unclosed parenthesis" $ do
      result <- try (parseStringWrapper "(+ 1 2") :: IO (Either SomeException [SExpr])
      result `shouldSatisfy` isLeft

    it "fails on extra closing parenthesis" $ do
      result <- try (parseStringWrapper "(+ 1 2))") :: IO (Either SomeException [SExpr])
      result `shouldSatisfy` isLeft

    it "fails on mismatched parentheses" $ do
      result <- try (parseStringWrapper "((+ 1 2)") :: IO (Either SomeException [SExpr])
      result `shouldSatisfy` isLeft

    it "fails on invalid input" $ do
      result <- try (parseStringWrapper "@#$") :: IO (Either SomeException [SExpr])
      result `shouldSatisfy` isLeft

  describe "Parser - Complex expressions" $ do
    it "parses a define expression" $ do
      result <- parseStringWrapper "(define x 42)"
      result `shouldBe` [SList [SSymbol (SymbolName "define"), SSymbol (SymbolName "x"), SInteger 42]]

    it "parses a lambda expression" $ do
      result <- parseStringWrapper "(lambda (x) (* x x))"
      result `shouldBe` [SList [SSymbol (SymbolName "lambda"), SList [SSymbol (SymbolName "x")], SList [SSymbol (SymbolName "*"), SSymbol (SymbolName "x"), SSymbol (SymbolName "x")]]]

    it "parses a complete program" $ do
      result <- parseStringWrapper "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))"
      result
        `shouldBe` [ SList
                       [ SSymbol (SymbolName "define"),
                         SSymbol (SymbolName "factorial"),
                         SList
                           [ SSymbol (SymbolName "lambda"),
                             SList [SSymbol (SymbolName "n")],
                             SList
                               [ SSymbol (SymbolName "if"),
                                 SList [SSymbol (SymbolName "="), SSymbol (SymbolName "n"), SInteger 0],
                                 SInteger 1,
                                 SList [SSymbol (SymbolName "*"), SSymbol (SymbolName "n"), SList [SSymbol (SymbolName "factorial"), SList [SSymbol (SymbolName "-"), SSymbol (SymbolName "n"), SInteger 1]]]
                               ]
                           ]
                       ]
                   ]

  describe "Parser - File parsing" $ do
    it "parse a simple file" $ do
      resut <- parseFileWrapper "tests/fixtures/testParseFile.ss"
      resut `shouldBe` [SList [SSymbol (SymbolName "define"), SSymbol (SymbolName "x"), SInteger 42]]

    it "fails on non-existent file" $ do
      result <- try (parseFileWrapper "tests/fixtures/nonExistent.ss") :: IO (Either SomeException [SExpr])
      result `shouldSatisfy` isLeft

    it "fails on invalid file content" $ do
      result <- try (parseFileWrapper "tests/fixtures/testInvalidFile.ss") :: IO (Either SomeException [SExpr])
      result `shouldSatisfy` isLeft
