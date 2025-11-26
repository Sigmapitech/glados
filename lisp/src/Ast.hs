{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Abstract Syntax Tree definitions for the Lisp interpreter.
-- This module defines the core data types for representing parsed S-expressions,
-- AST nodes, runtime values, and environments.
module Ast where

import Data.String (IsString (..))

newtype SymbolName = SymbolName String
  deriving (Show, Eq, Ord, IsString)

newtype ErrorMsg = ErrorMsg String
  deriving (Show, Eq, IsString)

unSymbolName :: SymbolName -> String
unSymbolName (SymbolName s) = s

unErrorMsg :: ErrorMsg -> String
unErrorMsg (ErrorMsg s) = s

symbolToVar :: SymbolName -> VarName
symbolToVar (SymbolName s) = VarName s

data SExpr
  = SInteger Integer
  | SSymbol SymbolName
  | SBool Bool
  | SList [SExpr]
  deriving (Show, Eq)

type Result a = Either ErrorMsg a

type ParseResult = Result SExpr

mkError :: String -> ErrorMsg
mkError = ErrorMsg

defineSymbol, lambdaSymbol, ifSymbol :: SymbolName
defineSymbol = SymbolName "define"
lambdaSymbol = SymbolName "lambda"
ifSymbol = SymbolName "if"
