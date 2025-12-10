module Error where

import Text.Megaparsec (ShowErrorComponent (..))

-- | Custom Error Type for Lexical Errors
data GLaDOSError
  = ErrUnclosedComment
  | ErrUnclosedString
  | ErrInvalidChar Char
  | ErrGeneric String
  deriving (Show, Eq, Ord)

-- | ANSI Color Codes
bold, red, green, yellow, cyan, reset :: String
bold = "\ESC[1m"
red = "\ESC[1;31m"
green = "\ESC[1;32m"
yellow = "\ESC[1;33m"
cyan = "\ESC[1;36m"
reset = "\ESC[0m"

instance ShowErrorComponent GLaDOSError where
  showErrorComponent ErrUnclosedComment =
    red
      ++ "✖ Lexical Error: "
      ++ reset
      ++ "Unclosed block comment.\n"
      ++ cyan
      ++ "  Hint: "
      ++ reset
      ++ "You opened a comment with "
      ++ bold
      ++ "/*"
      ++ reset
      ++ " but never closed it with "
      ++ bold
      ++ "*/"
      ++ reset
      ++ "."
  showErrorComponent ErrUnclosedString =
    red
      ++ "✖ Lexical Error: "
      ++ reset
      ++ "String literal is missing a closing quote.\n"
      ++ cyan
      ++ "  Hint: "
      ++ reset
      ++ "Strings must end with "
      ++ bold
      ++ "\""
      ++ reset
      ++ "."
  showErrorComponent (ErrInvalidChar c) =
    red
      ++ "✖ Lexical Error: "
      ++ reset
      ++ "Unexpected character "
      ++ yellow
      ++ "'"
      ++ [c]
      ++ "'"
      ++ reset
      ++ ".\n"
      ++ cyan
      ++ "  Hint: "
      ++ reset
      ++ "This character is not valid in this language."
  showErrorComponent (ErrGeneric msg) =
    red ++ "✖ Error: " ++ reset ++ msg
