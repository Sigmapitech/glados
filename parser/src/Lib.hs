module Lib (lexString, lexFile) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lexer (parseRawTokens)
import Text.Megaparsec (errorBundlePretty, runParser)
import Tokens (Token)

-- | ANSI Escape Codes for styling
esc :: String -> String
esc code = "\ESC[" ++ code ++ "m"

reset, bold, blue, red, magenta :: String
reset = esc "0"
bold = esc "1"
red = esc "31"
blue = esc "34"
magenta = esc "35"

-- | Scans the Megaparsec error string and colors key symbols
styleOutput :: String -> String
styleOutput raw = unlines $ map colorizeLine (lines raw)
  where
    colorizeLine :: String -> String
    colorizeLine line
      | "tests/" `isSubsequenceOf` line = bold ++ magenta ++ line ++ reset
      | otherwise = concatMap colorizeChar line

    colorizeChar :: Char -> String
    colorizeChar c
      | c == '|' = bold ++ blue ++ "|" ++ reset
      | c == '^' = bold ++ red ++ "^" ++ reset
      | otherwise = [c]

    isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
    isSubsequenceOf [] _ = True
    isSubsequenceOf _ [] = False
    isSubsequenceOf a@(x : xs) (y : ys)
      | x == y = isSubsequenceOf xs ys
      | otherwise = isSubsequenceOf a ys

-- | Public API
lexInternal :: String -> String -> Either String [Token]
lexInternal sourceName input =
  case runParser parseRawTokens sourceName (T.pack input) of
    Right tokens -> Right tokens
    Left bundle -> Left (styleOutput $ errorBundlePretty bundle)

lexString :: String -> Either String [Token]
lexString = lexInternal "<string>"

lexFile :: FilePath -> IO (Either String [Token])
lexFile path = do
  contentOrErr <- try (TIO.readFile path) :: IO (Either IOException Text)
  case contentOrErr of
    Left ioErr -> return $ Left ("File Error: " ++ show ioErr)
    Right content -> return $ case runParser parseRawTokens path content of
      Right tokens -> Right tokens
      Left bundle -> Left (styleOutput $ errorBundlePretty bundle)
