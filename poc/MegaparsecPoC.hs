module MegaparsecPoC where

import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    ShowErrorComponent (showErrorComponent),
    between,
    customFailure,
    empty,
    errorBundlePretty,
    many,
    oneOf,
    runParser,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

-- Custom error
data GladosError
  = IllegalNumber Integer
  | CustomError String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent GladosError where
  showErrorComponent (IllegalNumber n) = "The number " ++ show n ++ " is forbidden by GLaDOS."
  showErrorComponent (CustomError msg) = msg

-- The Parser type uses Void for standard text, or GladosError for custom logic
type Parser = Parsec GladosError String

-- AST
data SExpr
  = AtomInt Integer
  | AtomSym String
  | List [SExpr]
  deriving (Show, Eq)

-- Lexer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

identifier :: Parser String
identifier = lexeme $ do
  first <- letterChar <|> oneOf "!$%&|*+-/:<=>?@^_~"
  rest <- many (alphaNumChar <|> oneOf "!$%&|*+-/:<=>?@^_~")
  return (first : rest)

-- Parser Logic
parseAtom :: Parser SExpr
parseAtom = try (integer >>= checkInt) <|> AtomSym <$> identifier
  where
    checkInt :: Integer -> Parser SExpr
    checkInt 0 = customFailure (IllegalNumber 0)
    checkInt n = pure (AtomInt n)

parseList :: Parser SExpr
parseList = List <$> parens (many parseExpr)

parseExpr :: Parser SExpr
parseExpr = parseAtom <|> parseList

parseLisp :: Parser [SExpr]
parseLisp = sc >> many parseExpr <* eof

-- Runner
parseString :: String -> IO [SExpr]
parseString input = case runParser parseLisp "source_file" input of
  Left bundle -> do
    hPutStrLn stderr (errorBundlePretty bundle)
    exitWith (ExitFailure 84)
  Right ast -> return ast
