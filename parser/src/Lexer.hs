module Lexer where

import AST.Types.Common (FilePath' (..), Located (..), SourcePos (..), SourceSpan (..))
import Control.Monad (void)
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
import Tokens (Token, TokenConent (..))

toMyPos :: MP.SourcePos -> Int -> SourcePos
toMyPos sp off =
  SourcePos
    { posFile = FilePath' (T.pack (MP.sourceName sp)),
      posLine = fromIntegral (MP.unPos (MP.sourceLine sp)),
      posColumn = fromIntegral (MP.unPos (MP.sourceColumn sp)),
      posOffset = fromIntegral off
    }

type Parser = Parsec GLaDOSError String

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

withLoc :: Parser TokenConent -> Parser Token
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

tokInt :: Parser Token
tokInt = withLoc $ lexeme $ TokInt <$> L.decimal

tokString :: Parser Token
tokString = withLoc $ lexeme $ do
  startPos <- getOffset -- 1. Capture position at start quote
  void (char '"')
  content <- go startPos
  return $ TokString content
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

-- | Parse Identifiers/Keywords
tokWord :: Parser Token
tokWord = withLoc $ lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let word = first : rest
  if word `elem` reservedNames
    then return $ TokKeyword word
    else return $ TokIdentifier word

-- | Fallback for Illegal Characters
tokIllegal :: Parser Token
tokIllegal = withLoc $ do
  c <- lookAhead anySingle
  customFailure (ErrInvalidChar c)

-- | Choice of valid tokens
validToken :: Parser Token
validToken =
  tokInt
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
