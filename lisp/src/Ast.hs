{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Abstract Syntax Tree definitions for the Lisp interpreter.
-- This module defines the core data types for representing parsed S-expressions,
-- AST nodes, runtime values, and environments.
module Ast where

import Data.String (IsString (..))



-- | Symbol from parser (before semantic analysis)
newtype SymbolName = SymbolName String
  deriving (Show, Eq, Ord, IsString)

-- | Error messages
newtype ErrorMsg = ErrorMsg String
  deriving (Show, Eq, IsString)



-- | Extract the raw string from a SymbolName
unSymbolName :: SymbolName -> String
unSymbolName (SymbolName s) = s

-- | Extract the raw string from an ErrorMsg
unErrorMsg :: ErrorMsg -> String
unErrorMsg (ErrorMsg s) = s



-- | Convert a symbol name to a variable name
symbolToVar :: SymbolName -> VarName
symbolToVar (SymbolName s) = VarName s

-- | S-expression representation from the parser
data SExpr
  = -- | Integer literal
    SInteger Integer
  | -- | Symbol (variable or function name)
    SSymbol SymbolName
  | -- | Boolean literal
    SBool Bool
  | -- | List of S-expressions
    SList [SExpr]
  deriving (Show, Eq)


-- | Generic result type (either error or success)
type Result a = Either ErrorMsg a

-- | Result type for parsing operations
type ParseResult = Either ErrorMsg SExpr


-- | Helper to create error messages
mkError :: String -> ErrorMsg
mkError = ErrorMsg

-- | Special form keywords used by the parser
defineSymbol, lambdaSymbol, ifSymbol :: SymbolName
defineSymbol = SymbolName "define"
lambdaSymbol = SymbolName "lambda"
ifSymbol = SymbolName "if"
