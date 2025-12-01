module SExpr where

data SExpr
  = SInteger Integer
  | SSymbol String
  | SBool Bool
  | SList [SExpr]
  deriving (Show, Eq)
