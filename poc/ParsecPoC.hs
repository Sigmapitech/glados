module ParsecPoC where

import Data.Functor.Identity (Identity)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

-- AST Definition
data SExpr
  = AtomInt Integer
  | AtomSym String
  | List [SExpr]
  deriving (Show, Eq)

-- Lexer
lexerStyle :: Token.GenLanguageDef String u Identity
lexerStyle =
  emptyDef
    { Token.commentLine = ";",
      Token.identStart = letter <|> oneOf "!$%&|*+-/:<=>?@^_~",
      Token.identLetter = alphaNum <|> oneOf "!$%&|*+-/:<=>?@^_~"
    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser lexerStyle

parens :: Parser a -> Parser a
parens = Token.parens lexer

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Parser
parseAtom :: Parser SExpr
parseAtom =
  (AtomInt <$> integer)
    <|> (AtomSym <$> identifier)
    <?> "atom"

parseList :: Parser SExpr
parseList =
  List <$> parens (many parseExpr)
    <?> "list"

parseExpr :: Parser SExpr
parseExpr = parseAtom <|> parseList

parseLisp :: Parser [SExpr]
parseLisp = whiteSpace >> many parseExpr <* eof

-- Runner
parseString :: String -> IO [SExpr]
parseString input = case parse parseLisp "source_file" input of
  Left err -> do
    hPutStrLn stderr $ "Parse error: " ++ show err
    exitWith (ExitFailure 84)
  Right ast -> return ast

main :: IO ()
main = do
  putStrLn "--- Parsec Test ---"
  res <- parseString "(define x 42)"
  print res
  _ <- parseString "(unclosed parenthesis" -- Will crash with 84
  return ()
