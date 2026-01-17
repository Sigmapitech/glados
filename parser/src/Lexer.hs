{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import AST.Types.Common (FilePath' (..), Located (..), SourcePos (..), SourceSpan (..))
import AST.Types.Literal (IntBase (..))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Error (GLaDOSError (..))
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead),
    Parsec,
    anySingle,
    between,
    choice,
    customFailure,
    getOffset,
    many,
    optional,
    setOffset,
    (<|>),
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    newline,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Tokens (Token, TokenContent (..))

toMyPos :: MP.SourcePos -> Int -> SourcePos
toMyPos sp off =
  SourcePos
    { posFile = FilePath' (T.pack (MP.sourceName sp)),
      posLine = fromIntegral (MP.unPos (MP.sourceLine sp)),
      posColumn = fromIntegral (MP.unPos (MP.sourceColumn sp)),
      posOffset = fromIntegral off
    }

type Parser = Parsec GLaDOSError Text

-- | Space Consumer
--   Manually handles block comments to catch unclosed errors at the start position.
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//" <|> L.skipLineComment "#")
    skipBlockComment
  where
    skipBlockComment = do
      startPos <- getOffset -- 1. Capture position at '/*'
      void (string "/*")
      go startPos
      where
        go startPos = do
          done <- optional (string "*/")
          case done of
            Just _ -> return ()
            Nothing -> do
              isEof <- checkEOF
              if isEof
                then do
                  setOffset startPos -- 2. Jump back to '/*'
                  customFailure ErrUnclosedComment
                else anySingle >> go startPos

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

withLoc :: Parser TokenContent -> Parser Token
withLoc p = do
  startPos <- MP.getSourcePos
  startOff <- getOffset

  val <- p

  endPos <- MP.getSourcePos
  endOff <- getOffset
  sc
  let spanPos =
        SourceSpan
          { spanStart = toMyPos startPos startOff,
            spanEnd = toMyPos endPos endOff
          }
   in return (Located spanPos val)

reservedNames :: [Text]
reservedNames =
  [ "import",
    "fn",
    "int",
    "float",
    "bool",
    "str",
    "void",
    "return",
    "if",
    "else",
    "while",
    "for",
    "break",
    "continue",
    "const"
  ]

tokInt :: Parser Token
tokInt = withLoc $ lexeme $ do
  prefix <- optional (char '0')
  case prefix of
    Just _ -> do
      baseChar <- optional (char 'x' <|> char 'X' <|> char 'o' <|> char 'O' <|> char 'b' <|> char 'B')
      case baseChar of
        Just 'x' -> TokInt <$> L.hexadecimal <*> pure BaseHex
        Just 'X' -> TokInt <$> L.hexadecimal <*> pure BaseHex
        Just 'o' -> TokInt <$> L.octal <*> pure BaseOct
        Just 'O' -> TokInt <$> L.octal <*> pure BaseOct
        Just 'b' -> TokInt <$> L.binary <*> pure BaseBin
        Just 'B' -> TokInt <$> L.binary <*> pure BaseBin
        _ -> do
          rest <- optional L.decimal
          case rest of
            Just n -> return $ TokInt n BaseDec
            Nothing -> return $ TokInt 0 BaseDec
    Nothing -> TokInt <$> L.decimal <*> pure BaseDec

tokString :: Parser Token
tokString = withLoc $ lexeme $ do
  startPos <- getOffset -- 1. Capture position at start quote
  void (char '"')
  content <- go startPos
  return $ TokString (T.pack content)
  where
    go startPos = do
      done <- optional (char '"')
      case done of
        Just _ -> return ""
        Nothing -> do
          isNewline <- optional newline
          isEof <- checkEOF
          case (isNewline, isEof) of
            (Just _, _) -> do
              setOffset startPos -- 2. Jump back to start quote
              customFailure ErrUnclosedString
            (_, True) -> do
              setOffset startPos -- 2. Jump back to start quote
              customFailure ErrUnclosedString
            _ -> do
              c <- L.charLiteral
              cs <- go startPos
              return (c : cs)

-- | Parse Symbols
tokSymbol :: Parser Token
tokSymbol = withLoc $ lexeme $ do
  sym <-
    choice $
      -- Longest operators first (3 chars)
      [ string "<<=",
        string ">>="
      ]
        ++ [ string "==",
             string "!=",
             string "<=",
             string ">=",
             string "&&",
             string "||",
             string "->",
             string "<<",
             string ">>",
             string "++",
             string "--"
           ]
        -- Then compound assignments (2 chars)
        ++ [string (T.pack [op, '=']) | op <- "+-*~/%&|^"]
        -- Then single-char operators and delimiters
        ++ [string (T.singleton c) | c <- "(){}[];~:=+-*/%<>^,!&|."]
  return $ TokSymbol sym

-- | Parse Identifiers/Keywords
tokWord :: Parser Token
tokWord = withLoc $ lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let firstPart = first : rest

  moreParts <- many $ MP.try $ do
    _ <- char '.'
    f <- letterChar <|> char '_'
    r <- many (alphaNumChar <|> char '_')
    return ('.' : f : r)

  let word = T.pack (firstPart ++ concat moreParts)

  if word `elem` reservedNames
    then return $ TokKeyword word
    else return $ TokIdentifier word

tokBool :: Parser Token
tokBool = withLoc $ lexeme $ do
  b <- (string "True" >> return True) <|> (string "False" >> return False)
  return $ TokBool b

-- | Fallback for Illegal Characters
tokIllegal :: Parser Token
tokIllegal = withLoc $ do
  c <- lookAhead anySingle
  customFailure (ErrInvalidChar c)

-- | Choice of valid tokens
validToken :: Parser Token
validToken =
  tokInt
    <|> tokBool
    <|> tokWord
    <|> tokSymbol

-- | Main Parse Loop
parseRawTokens :: Parser [Token]
parseRawTokens = between sc eof loop
  where
    loop = do
      isEof <- checkEOF
      if isEof
        then return []
        else do
          c <- lookAhead anySingle

          tok <- case c of
            '"' -> tokString
            _ -> do
              res <- optional validToken
              maybe tokIllegal return res

          toks <- loop
          return (tok : toks)

checkEOF :: Parser Bool
checkEOF = do
  res <- optional eof
  case res of
    Just _ -> return True
    Nothing -> return False
