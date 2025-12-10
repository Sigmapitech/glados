module Tokens where

data Token
  = TokKeyword String
  | TokIdentifier String
  | TokSymbol String
  | TokInt Integer
  | TokBool Bool
  | TokString String
  | TokChar Char
  | TokEOF
  deriving (Show, Eq)
