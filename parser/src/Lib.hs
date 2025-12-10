module Lib (lexString, lexFile) where

import Control.Exception (IOException, try)
import Lexer (parseRawTokens)
import Text.Megaparsec (errorBundlePretty, runParser)
import Tokens (Token)

-- | Pure function: String -> Either Error [Token]
lexString :: String -> Either String [Token]
lexString input =
  case runParser parseRawTokens "<string>" input of
    Left bundle -> Left (errorBundlePretty bundle)
    Right tokens -> Right tokens

-- | IO function: FilePath -> IO (Either Error [Token])
-- Handles file reading errors safely.
lexFile :: FilePath -> IO (Either String [Token])
lexFile path = do
  -- Catch IO exceptions (like file not found)
  contentOrErr <- try (readFile path) :: IO (Either IOException String)
  case contentOrErr of
    Left ioErr -> return $ Left ("File Error: " ++ show ioErr)
    Right content ->
      return $ case runParser parseRawTokens path content of
        Left bundle -> Left (errorBundlePretty bundle)
        Right tokens -> Right tokens
