module Tokens where

-- | The raw lexical units (Tokens) of the language.
-- This is NOT the AST.
data Token
  = TokKeyword String
  | TokIdentifier String
  | TokSymbol String
  | TokInt Integer
  | TokBool Bool
  | TokString String
  | TokEOF
  deriving (Show, Eq)
