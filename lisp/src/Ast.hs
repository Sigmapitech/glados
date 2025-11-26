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

-- | Runtime values produced by evaluation
data Value
  = -- | Integer value
    VInt Integer
  | -- | Boolean value
    VBool Bool
  | -- | Function closure
    VFunction [ParamName] Ast Environment
  | -- | Unit value
    VUnit
  deriving (Show)

-- | Make Value an instance of Eq (useful for testing).
-- Note: Functions are not comparable and always return False.
instance Eq Value where
  (VInt a) == (VInt b) = a == b
  (VBool a) == (VBool b) = a == b
  VUnit == VUnit = True
  (VFunction {}) == (VFunction {}) = False
  _ == _ = False

-- | A binding associates a variable name with its value
type Binding = (VarName, Value)

-- | Environment maps variable names to their values
type Environment = [Binding]

-- | Create an empty environment
emptyEnv :: Environment
emptyEnv = []

-- | Add a new binding to the environment
extendEnv :: VarName -> Value -> Environment -> Environment
extendEnv name value env = (name, value) : env

-- | Look up a variable in the environment
lookupEnv :: VarName -> Environment -> Maybe Value
lookupEnv = lookup
-- | Generic result type (either error or success)
type Result a = Either ErrorMsg a

-- | Result type for parsing operations
type ParseResult = Either ErrorMsg SExpr

-- | Result type for AST conversion operations
type ConvertResult = Either ErrorMsg Ast

-- | Result of evaluating to a value
type ValueResult = Either ErrorMsg Value

-- | Complete evaluation result (value + final environment)
--
-- The type parameter 'a' allows us to use this for different return types:
--   - Evaluator Value      (most common)
--   - Evaluator [Value]    (for evaluating lists)
--   - Evaluator Bool       (for internal checks)
--   - etc.
type EvalResult a = (Either ErrorMsg a, Environment)

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
