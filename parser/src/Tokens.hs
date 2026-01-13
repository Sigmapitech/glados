module Tokens where

import AST.Types.Common (Located)

data TokenConent
  = TokKeyword String
  | TokIdentifier String
  | TokSymbol String
  | TokInt Integer
  | TokBool Bool
  | TokString String
  | TokChar Char
  | TokEOF
  deriving (Show, Eq, Ord)

type Token = Located TokenConent
