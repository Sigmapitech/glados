module Lib (lexString, lexFile) where

import Control.Exception (IOException, try)
import Lexer (parseRawTokens)
import Text.Megaparsec (errorBundlePretty, runParser)
import Tokens (Token)

lexInternal :: String -> String -> Either String [Token]
lexInternal sourceName input = either (Left . errorBundlePretty) Right $ runParser parseRawTokens sourceName input

-- | Pure function: String -> Either Error [Token]
lexString :: String -> Either String [Token]
lexString = lexInternal "<string>"

-- | IO function: FilePath -> IO (Either Error [Token])
-- Handles file reading errors safely.
lexFile :: FilePath -> IO (Either String [Token])
lexFile path = do
  -- Catch IO exceptions (like file not found)
  contentOrErr <- try (readFile path) :: IO (Either IOException String)
  case contentOrErr of
    Left ioErr -> return $ Left ("File Error: " ++ show ioErr)
    Right content ->
      return $ lexInternal path content
