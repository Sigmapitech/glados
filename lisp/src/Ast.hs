-- | Abstract Syntax Tree definitions for the Lisp interpreter.
-- This module defines the core data types for representing parsed S-expressions,
-- AST nodes, runtime values, and environments.
module Ast where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, runState)
import Data.String (IsString (..))

newtype VarName = VarName String
  deriving (Show, Eq, Ord, IsString)

newtype ParamName = ParamName String
  deriving (Show, Eq, Ord, IsString)

newtype SymbolName = SymbolName String
  deriving (Show, Eq, Ord, IsString)

newtype ErrorMsg = ErrorMsg String
  deriving (Show, Eq, IsString)

unVarName :: VarName -> String
unVarName (VarName s) = s

unParamName :: ParamName -> String
unParamName (ParamName s) = s

unSymbolName :: SymbolName -> String
unSymbolName (SymbolName s) = s

unErrorMsg :: ErrorMsg -> String
unErrorMsg (ErrorMsg s) = s

paramToVar :: ParamName -> VarName
paramToVar (ParamName s) = VarName s

varToParam :: VarName -> ParamName
varToParam (VarName s) = ParamName s

symbolToVar :: SymbolName -> VarName
symbolToVar (SymbolName s) = VarName s

data SExpr
  = SInteger Integer
  | SSymbol SymbolName
  | SBool Bool
  | SList [SExpr]
  deriving (Show, Eq)

data Ast
  = LiteralInt Integer
  | LiteralBool Bool
  | VariableRef VarName
  | -- | Variable definition (Function or value)
    Define VarName Ast
  | Lambda [ParamName] Ast
  | Call Ast [Ast]
  | If {ifCond :: Ast, ifThen :: Ast, ifElse :: Ast}
  deriving (Show, Eq)

data RuntimeValue
  = VInt Integer
  | VBool Bool
  | VProcedure [ParamName] Ast Environment
  | VUnit -- Represents 'void' or 'no value': "() <- unit"
  deriving (Show)

-- | Make RuntimeValue an instance of Eq (useful for testing).
-- Note: Functions are not comparable and always return False.
instance Eq RuntimeValue where
  (VInt a) == (VInt b) = a == b
  (VBool a) == (VBool b) = a == b
  VUnit == VUnit = True
  (VProcedure {}) == (VProcedure {}) = False
  _ == _ = False

type Binding = (VarName, RuntimeValue)

type Environment = [Binding]

emptyEnv :: Environment
emptyEnv = []

extendEnv :: VarName -> RuntimeValue -> Environment -> Environment
extendEnv name value env = (name, value) : env

lookupEnv :: VarName -> Environment -> Maybe RuntimeValue
lookupEnv = lookup

type Result a = Either ErrorMsg a

type ParseResult = Result SExpr

type ConvertResult = Result Ast

type ValueResult = Result RuntimeValue

type EvalResult = (ValueResult, Environment)

-- | The Evaluator monad combines error handling and state management
--
-- This is a monad transformer stack:
--   - ExceptT: provides error handling (Either ErrorMsg)
--   - State: provides environment threading
type Evaluator = ExceptT ErrorMsg (State Environment) RuntimeValue

-- | Run an evaluator computation with an initial environment (unwrap the monad stack)
-- Returns both the result and the final environment
runEvaluator :: Evaluator -> Environment -> EvalResult
runEvaluator computation = runState $ runExceptT computation

--                         ^^^^^^^^  ^^^^^^^^^^^
--                         unwrap    unwrap
--                         State     ExceptT

mkError :: String -> ErrorMsg
mkError = ErrorMsg

throwEvalError :: String -> Evaluator
throwEvalError = throwError . mkError

liftError :: String -> Either String a -> Result a
liftError context (Left err) = Left $ mkError $ context ++ ": " ++ err
liftError _ (Right val) = Right val

addErrContext :: String -> Result a -> Result a
addErrContext context (Left (ErrorMsg msg)) = Left $ mkError $ context ++ ": " ++ msg
addErrContext _ result = result

builtinPlus, builtinMinus, builtinMult :: VarName
builtinPlus = VarName "+"
builtinMinus = VarName "-"
builtinMult = VarName "*"

builtinDiv, builtinMod :: VarName
builtinDiv = VarName "div"
builtinMod = VarName "mod"

builtinEq, builtinLt :: VarName
builtinEq = VarName "eq?"
builtinLt = VarName "<"

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

defineSymbol, lambdaSymbol, ifSymbol :: SymbolName
defineSymbol = SymbolName "define"
lambdaSymbol = SymbolName "lambda"
ifSymbol = SymbolName "if"
