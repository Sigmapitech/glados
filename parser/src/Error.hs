{-# LANGUAGE InstanceSigs #-}

{- HLINT ignore "Use newtype instead of data" -}

module Error where

import Text.Megaparsec (ShowErrorComponent (..))

-- | Custom Error Type
data GLaDOSError
  = ErrUnclosedComment
  | ErrUnclosedString
  | ErrInvalidChar Char
  | ErrGeneric String
  deriving (Show, Eq, Ord)

data ParseError
  = TypeError String
  deriving (Show, Eq, Ord)

esc :: String -> String
esc code = "\ESC[" ++ code ++ "m"

reset, bold :: String
reset = esc "0"
bold = esc "1"

red, cyan, yellow, magenta :: String
red = esc "31"
cyan = esc "36"
yellow = esc "33"
magenta = esc "35"

-- Symbols for highlighting code in messages
codeSym :: String -> String
codeSym s = bold ++ magenta ++ s ++ reset

instance ShowErrorComponent GLaDOSError where
  showErrorComponent :: GLaDOSError -> String
  showErrorComponent ErrUnclosedComment =
    unlines
      [ "",
        bold ++ red ++ "✖ Lexical Error: " ++ reset ++ "Unclosed block comment.",
        bold ++ cyan ++ "  ℹ Hint: " ++ reset ++ cyan ++ "You opened a comment with " ++ codeSym "/*" ++ cyan ++ " but never closed it." ++ reset,
        cyan ++ "          Look for the matching " ++ codeSym "*/" ++ cyan ++ " or close it before EOF." ++ reset
      ]
  showErrorComponent ErrUnclosedString =
    unlines
      [ "",
        bold ++ red ++ "✖ Lexical Error: " ++ reset ++ "String literal is missing a closing quote.",
        bold ++ cyan ++ "  ℹ Hint: " ++ reset ++ cyan ++ "Strings must end with " ++ codeSym "\"" ++ cyan ++ " on the same line." ++ reset
      ]
  showErrorComponent (ErrInvalidChar c) =
    let hintMsg = case c of
          '`' -> "Backticks are not valid. Did you mean a single quote " ++ codeSym "'" ++ cyan ++ "?"
          '\'' -> "It looks like an empty or broken character. characters look like " ++ codeSym "'a'" ++ cyan ++ "."
          ';' -> "Semicolons are valid, but appeared where an expression was expected."
          _ -> "This character is not recognized in the language syntax."
     in unlines
          [ "",
            bold ++ red ++ "✖ Lexical Error: " ++ reset ++ "Unexpected character " ++ bold ++ yellow ++ "'" ++ [c] ++ "'" ++ reset ++ ".",
            bold ++ cyan ++ "  ℹ Hint: " ++ reset ++ cyan ++ hintMsg ++ reset
          ]
  showErrorComponent (ErrGeneric msg) =
    unlines
      [ "",
        bold ++ red ++ "✖ Error: " ++ reset ++ msg
      ]
