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

-- 1. Custom Error Component (The "Own Error Handling" part)
-- You can define specific error cases that ParseErrorBundle doesn't cover by default.
data GladosError
  = IllegalNumber Integer -- Example: We don't like number 0
  | CustomError String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent GladosError where
  showErrorComponent (IllegalNumber n) = "The number " ++ show n ++ " is forbidden by GLaDOS."
  showErrorComponent (CustomError msg) = msg

-- The Parser type uses Void for standard text, or GladosError for custom logic
type Parser = Parsec GladosError String

-- 2. AST
data SExpr
  = AtomInt Integer
  | AtomSym String
  | List [SExpr]
  deriving (Show, Eq)

-- 3. Lexer
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

-- 4. Parser Logic
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

-- 5. Entry Point & Fancy Error Display
parseString :: String -> IO [SExpr]
parseString input = case runParser parseLisp "source_file" input of
  Left bundle -> do
    -- errorBundlePretty gives very nice, annotated errors (like Rust/Clang)
    hPutStrLn stderr (errorBundlePretty bundle)
    exitWith (ExitFailure 84)
  Right ast -> return ast

main :: IO ()
main = do
  putStrLn "--- Megaparsec Test ---"
  res <- parseString "(define x 42) (print x)"
  print res

  putStrLn "\n--- Testing Custom Error (0 is forbidden) ---"
  -- This will trigger our custom GladosError
  _ <- parseString "(val 0)"
  return ()
