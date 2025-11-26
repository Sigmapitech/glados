{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Abstract Syntax Tree definitions for the Lisp interpreter.
-- This module defines the core data types for representing parsed S-expressions,
-- AST nodes, runtime values, and environments.
module Ast where

import Data.String (IsString (..))

-- | Variable name in the program
newtype VarName = VarName String
  deriving (Show, Eq, Ord, IsString)

-- | Parameter name in lambda/function definition
newtype ParamName = ParamName String
  deriving (Show, Eq, Ord, IsString)

-- | Symbol from parser (before semantic analysis)
newtype SymbolName = SymbolName String
  deriving (Show, Eq, Ord, IsString)

-- | Error messages
newtype ErrorMsg = ErrorMsg String
  deriving (Show, Eq, IsString)

-- | Extract the raw string from a VarName
unVarName :: VarName -> String
unVarName (VarName s) = s

-- | Extract the raw string from a ParamName
unParamName :: ParamName -> String
unParamName (ParamName s) = s

-- | Extract the raw string from a SymbolName
unSymbolName :: SymbolName -> String
unSymbolName (SymbolName s) = s

-- | Extract the raw string from an ErrorMsg
unErrorMsg :: ErrorMsg -> String
unErrorMsg (ErrorMsg s) = s

-- | Convert a parameter name to a variable name
paramToVar :: ParamName -> VarName
paramToVar (ParamName s) = VarName s

-- | Convert a variable name to a parameter name
varToParam :: VarName -> ParamName
varToParam (VarName s) = ParamName s

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

-- | Abstract Syntax Tree representation after semantic analysis
data Ast
  = -- | Integer literal
    LiteralInt Integer
  | -- | Boolean literal
    LiteralBool Bool
  | -- | Variable reference
    Variable VarName
  | -- | Variable definition (Function or value)
    Define VarName Ast
  | -- | Anonymous function
    Lambda [ParamName] Ast
  | -- | Function call
    Call Ast [Ast]
  | -- | Conditional expression
    If {ifCond :: Ast, ifThen :: Ast, ifElse :: Ast}
  deriving (Show, Eq)

-- | Generic result type (either error or success)
type Result a = Either ErrorMsg a

-- | Result type for parsing operations
type ParseResult = Either ErrorMsg SExpr

-- | Result type for AST conversion operations
type ConvertResult = Either ErrorMsg Ast

-- | Helper to create error messages
mkError :: String -> ErrorMsg
mkError = ErrorMsg


-- | Lift a string-based error into an ErrorMsg with context
liftError :: String -> Either String a -> Either ErrorMsg a
liftError context (Left err) = Left $ ErrorMsg $ context ++ ": " ++ err
liftError _ (Right val) = Right val

-- | Built-in arithmetic operators
builtinPlus, builtinMinus, builtinMult :: VarName
builtinPlus = VarName "+"
builtinMinus = VarName "-"
builtinMult = VarName "*"

-- | Built-in division and modulo operators
builtinDiv, builtinMod :: VarName
builtinDiv = VarName "div"
builtinMod = VarName "mod"

-- | Built-in comparison operators
builtinEq, builtinLt :: VarName
builtinEq = VarName "eq?"
builtinLt = VarName "<"

-- | Check if a variable name refers to a built-in function
isBuiltin :: VarName -> Bool
isBuiltin name =
  name
    `elem` [ builtinPlus,
             builtinMinus,
             builtinMult,
             builtinDiv,
             builtinMod,
             builtinEq,
             builtinLt
           ]

-- | Special form keywords used by the parser
defineSymbol, lambdaSymbol, ifSymbol :: SymbolName
defineSymbol = SymbolName "define"
lambdaSymbol = SymbolName "lambda"
ifSymbol = SymbolName "if"
