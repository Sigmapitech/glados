module Main where

import System.Exit (exitSuccess)
import Data.Maybe (fromJust)

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

printTree :: SExpr -> Maybe String
printTree (SInteger n) = Just (show n)
printTree (SSymbol s) = Just s
printTree (SList exprs) = Just $ "(" ++ unwords (map printSExpr exprs) ++ ")"
  where
    printSExpr expr = fromJust (printTree expr)


-- Abstract Syntax Tree definition
data AST
  = ASTInteger { intValue :: Integer }
  | ASTSymbol { symbolName :: String }
  | ASTBoolean { boolValue :: Bool }
  | Define { defName :: String, defValue :: AST }
  | Call { callFunction :: String, callArgs :: [AST] }
  deriving Show

sexprToAST :: SExpr -> Maybe AST
sexprToAST (SInteger n) = Just (ASTInteger n)
sexprToAST (SSymbol s) = Just (ASTSymbol s)
sexprToAST (SList [SSymbol "define", SSymbol name, valueExpr]) = do
  valueAST <- sexprToAST valueExpr
  Just (Define name valueAST)
sexprToAST (SList (SSymbol func : args)) = do
  argsAST <- mapM sexprToAST args
  Just (Call func argsAST)
sexprToAST _ = Nothing
main :: IO ()
main =
  exitSuccess
