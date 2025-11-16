module Main where

import System.Exit (exitSuccess)
-- S-Expression definition
data SExpr
  = SInteger Integer
  | SSymbol String
  | SList [SExpr]
  deriving Show

main :: IO ()
main =
  exitSuccess
