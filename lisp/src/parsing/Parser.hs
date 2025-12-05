module Parser (parseFile, parseString) where

import Ast (SExpr (..), SymbolName (..))
import Data.Void (Void)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    between,
    errorBundlePretty,
    many,
    oneOf,
    optional,
    runParser,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Lexer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockCommentNested "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parsers
integer :: Parser Integer
integer = lexeme $ do
  sign <- optional (char '+' <|> char '-')
  digits <- L.decimal
  return $ case sign of
    Just '-' -> negate digits
    _ -> digits

identifier :: Parser SymbolName
identifier = lexeme $ do
  first <- letterChar <|> oneOf ("!$%&|*+-/:<=>?@^_~" :: String)
  rest <- many (alphaNumChar <|> oneOf ("!$%&|*+-/:<=>?@^_~" :: String))
  return $ SymbolName (first : rest)

bool :: Parser Bool
bool =
  lexeme $
    (string "#t" >> return True)
      <|> (string "#f" >> return False)

parseAtom :: Parser SExpr
parseAtom =
  try (SBool <$> bool)
    <|> try (SInteger <$> integer)
    <|> (SSymbol <$> identifier)

parseList :: Parser SExpr
parseList = SList <$> parens (many parseSExpr)

parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> parseList

parseProgram :: Parser [SExpr]
parseProgram = sc *> many parseSExpr <* eof

-- Public API
parseString :: String -> IO [SExpr]
parseString input = case runParser parseProgram "<input>" input of
  Left bundle -> do
    hPutStrLn stderr (errorBundlePretty bundle)
    exitWith (ExitFailure 84)
  Right ast -> return ast

parseFile :: FilePath -> IO [SExpr]
parseFile path = do
  input <- readFile path
  case runParser parseProgram path input of
    Left bundle -> do
      hPutStrLn stderr (errorBundlePretty bundle)
      exitWith (ExitFailure 84)
    Right ast -> return ast
