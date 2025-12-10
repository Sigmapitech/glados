module Lexer where

-- FIX: hiding (Token) prevents the clash with your custom Token type from Tokens.hs

import Control.Monad (void)
import Error (GLaDOSError (..))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Tokens (Token (..))

type Parser = Parsec GLaDOSError String

-- | Space Consumer
-- Handles whitespace, single-line comments, block comments (with error), and preprocessor directives.
sc :: Parser ()
sc =
  L.space
    space1
    skipLineComment
    skipBlockComment
  where
    -- Standard C-style single line comment
    skipLineComment = L.skipLineComment "//" <|> L.skipLineComment "#"

    -- Block comment with custom error on unclosed EOF
    skipBlockComment = do
      void (string "/*")
      -- Consumes input until "*/" or EOF
      -- If EOF is hit before "*/", we throw the custom error
      let go = do
            done <- optional (string "*/")
            case done of
              Just _ -> return () -- Comment closed successfully
              Nothing -> do
                c <- optional anySingle
                case c of
                  Just _ -> go -- Keep consuming comment content
                  Nothing -> customFailure ErrUnclosedComment -- EOF hit!
      go

-- | Wrapper for lexemes
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

reservedNames :: [String]
reservedNames =
  [ "int",
    "char",
    "bool",
    "void",
    "return",
    "if",
    "else",
    "while",
    "for",
    "true",
    "false"
  ]

-- | 1. Parse Integers
tokInt :: Parser Token
tokInt = lexeme $ TokInt <$> L.decimal

-- | 2. Parse Strings (with custom error)
tokString :: Parser Token
tokString = lexeme $ do
  _ <- char '"'
  content <- manyTill L.charLiteral (char '"' <|> (newline >> customFailure ErrUnclosedString))
  return $ TokString content

-- | 3. Parse Symbols
tokSymbol :: Parser Token
tokSymbol = lexeme $ do
  -- We join the list of multi-char strings with the list of single-char strings
  -- Using (++) to combine the lists resolves the list comprehension syntax error
  sym <-
    choice $
      [ string "==",
        string "!=",
        string "<=",
        string ">=",
        string "&&",
        string "||",
        string "->",
        string "=>"
      ]
        ++ [string [c] | c <- "(){}[];=+-*/%<>,!&|"]
  return $ TokSymbol sym

-- | 4. Parse Identifiers/Keywords
tokWord :: Parser Token
tokWord = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let word = first : rest
  if word `elem` reservedNames
    then return $ TokKeyword word
    else return $ TokIdentifier word

-- | 5. Catch-all for Illegal Characters
-- This MUST be the last parser tried. It consumes 1 char and fails.
tokIllegal :: Parser Token
tokIllegal = do
  c <- anySingle
  customFailure (ErrInvalidChar c)

-- | The master lexer
parseToken :: Parser Token
parseToken =
  tokInt
    <|> tokString
    <|> tokWord
    <|> tokSymbol
    <|> tokIllegal -- <--- The Catch-All

parseRawTokens :: Parser [Token]
parseRawTokens = between sc eof (many parseToken)
