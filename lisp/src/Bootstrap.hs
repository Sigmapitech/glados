{-# LANGUAGE LambdaCase #-}

module Bootstrap
  ( SExpr (..),
    getInteger,
    getSymbol,
    getList,
    printTree,
    AST (..),
    sexprToAST,
    evalAST,
  )
where

import Data.Maybe (fromJust)

-- S-Expression definition
data SExpr
  = SInteger Integer
  | SSymbol String
  | SList [SExpr]
  deriving (Show)

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
  = ASTInteger {intValue :: Integer}
  | ASTSymbol {symbolName :: String}
  | ASTBoolean {boolValue :: Bool}
  | Define {defName :: String, defValue :: AST}
  | Call {callFunction :: String, callArgs :: [AST]}
  deriving (Show)

sexprToAST :: SExpr -> Maybe AST
sexprToAST (SInteger n) = Just (ASTInteger n)
sexprToAST (SSymbol s) = Just (ASTSymbol s)
sexprToAST (SList [SSymbol "define", SSymbol name, valueExpr]) = do
  valueAST <- sexprToAST valueExpr
  Just (Define name valueAST)
sexprToAST (SList (SSymbol "define" : _)) = Nothing
sexprToAST (SList (SSymbol func : args)) = do
  argsAST <- mapM sexprToAST args
  Just (Call func argsAST)
sexprToAST _ = Nothing

-- Helper function to extract integers from AST nodes
extractIntegers :: [AST] -> Maybe [Integer]
extractIntegers =
  mapM
    ( \case
        ASTInteger n -> Just n
        _ -> Nothing
    )

-- Helper function to apply binary operations
applyBinaryOp :: (Integer -> Integer -> Integer) -> [AST] -> Maybe AST
applyBinaryOp op args = do
  evaluatedArgs <- mapM evalAST args
  integers <- extractIntegers evaluatedArgs
  case integers of
    [x, y] -> Just (ASTInteger (op x y))
    _ -> Nothing

-- Helper function to apply binary operations with validation
applyBinaryOpWithCheck :: (Integer -> Integer -> Maybe Integer) -> [AST] -> Maybe AST
applyBinaryOpWithCheck op args = do
  evaluatedArgs <- mapM evalAST args
  integers <- extractIntegers evaluatedArgs
  case integers of
    [x, y] -> do
      result <- op x y
      Just (ASTInteger result)
    _ -> Nothing

evalAST :: AST -> Maybe AST
evalAST (ASTInteger n) = Just (ASTInteger n)
evalAST (ASTBoolean b) = Just (ASTBoolean b)
evalAST (ASTSymbol s) = Just (ASTSymbol s)
evalAST (Call "+" args) = applyBinaryOp (+) args
evalAST (Call "-" args) = applyBinaryOp (-) args
evalAST (Call "*" args) = applyBinaryOp (*) args
evalAST (Call "/" args) = applyBinaryOpWithCheck (\x y -> if y == 0 then Nothing else Just (div x y)) args
evalAST (Call "mod" args) = applyBinaryOpWithCheck (\x y -> if y == 0 then Nothing else Just (mod x y)) args
evalAST (Call func args) = do
  evaluatedArgs <- mapM evalAST args
  Just (Call func evaluatedArgs)
evalAST (Define name value) = do
  evaluatedValue <- evalAST value
  Just (Define name evaluatedValue)
