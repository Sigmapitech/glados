module Main where

import System.Exit (exitSuccess)
-- S-Expression definition
data SExpr
  = SInteger Integer
  | SSymbol String
  | SList [SExpr]
  deriving Show

getInteger :: SExpr -> Maybe Integer
getInteger (SInteger n) = Just n
getInteger _ = Nothing

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList xs) = Just xs
getList _ = Nothing
main :: IO ()
main =
  exitSuccess
