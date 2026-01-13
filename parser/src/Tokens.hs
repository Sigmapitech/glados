module Tokens where

import AST.Types.Common (Located)
import Data.Text (Text)

data TokenConent
  = TokKeyword Text
  | TokIdentifier Text
  | TokSymbol Text
  | TokInt Integer Int
  | TokBool Bool
  | TokString Text
  | TokChar Char
  | TokEOF
  deriving (Show, Eq, Ord)

type Token = Located TokenConent
