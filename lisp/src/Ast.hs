-- | Abstract Syntax Tree definitions for the Lisp interpreter.
-- This module defines the core data types for representing parsed S-expressions,
-- AST nodes, runtime values, and environments.
module Ast where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, runState)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))

newtype VarName = VarName String
  deriving (Show, Eq, IsString)

newtype ParamName = ParamName String
  deriving (Show, Eq, IsString)

newtype SymbolName = SymbolName String
  deriving (Show, Eq, IsString)

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
  deriving (Eq)

instance Show SExpr where
  show (SInteger n) = show n
  show (SSymbol (SymbolName s)) = s
  show (SBool True) = "#t"
  show (SBool False) = "#f"
  show (SList exprs) = "(" ++ unwords (map show exprs) ++ ")"

data Ast
  = LiteralInt Integer
  | LiteralBool Bool
  | VariableRef VarName
  | -- | Variable definition (Function or value)
    Define VarName Ast
  | Lambda [ParamName] Ast
  | Call Ast [Ast]
  | If {ifCond :: Ast, ifThen :: Ast, ifElse :: Ast}
  deriving (Eq)

instance Show Ast where
  show (LiteralInt n) = show n
  show (LiteralBool True) = "#t"
  show (LiteralBool False) = "#f"
  show (VariableRef name) = unVarName name
  show (Define name expr) = "(define " ++ unVarName name ++ " " ++ show expr ++ ")"
  show (Lambda params body) =
    "(lambda (" ++ unwords (map unParamName params) ++ ") " ++ show body ++ ")"
  show (Call func args) =
    "(" ++ show func ++ " " ++ unwords (map show args) ++ ")"
  show (If cond thenExpr elseExpr) =
    "(if " ++ show cond ++ " " ++ show thenExpr ++ " " ++ show elseExpr ++ ")"

data RuntimeValue
  = VInt Integer
  | VBool Bool
  | VProcedure [ParamName] Ast
  | VBuiltin BuitinOp
  | VUnit -- Represents 'void' or 'no value': "() <- unit"
  deriving (Eq)

instance Show RuntimeValue where
  show (VInt n) = show n
  show (VBool b) = if b then "#t" else "#f"
  show (VProcedure params _) = "#<procedure:" ++ show params ++ ">"
  show (VBuiltin op) = "#<builtin:" ++ show op ++ ">"
  show VUnit = "#<void>"

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

type ValueResult = NonEmpty (Result RuntimeValue)

-- | Unified result type that can be either single or multiple
type EvalResult = (ValueResult, Environment)

-- | The Evaluator monad combines error handling and state management
--
-- This is a monad transformer stack:
--   - ExceptT: provides error handling (Either ErrorMsg)
--   - State: provides environment threading
type Evaluator = ExceptT ErrorMsg (State Environment) RuntimeValue

-- | Run an evaluator computation with an initial environment (unwrap the monad stack)
-- Returns both the result and the final environment
runEvaluator :: Evaluator -> (Environment -> (Result RuntimeValue, Environment))
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

data BuitinOp
  = BPlus
  | BMinus
  | BMult
  | BDiv
  | BMod
  | BEq
  | BLt
  deriving (Eq)

instance Show BuitinOp where
  show BPlus = "+"
  show BMinus = "-"
  show BMult = "*"
  show BDiv = "div"
  show BMod = "mod"
  show BEq = "eq?"
  show BLt = "<"

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

allBuitinName :: [VarName]
allBuitinName =
  [ builtinPlus,
    builtinMinus,
    builtinMult,
    builtinDiv,
    builtinMod,
    builtinEq,
    builtinLt
  ]

isBuiltin :: VarName -> Bool
isBuiltin name = name `elem` allBuitinName

initialEnv :: Environment
initialEnv =
  [ (builtinPlus, VBuiltin BPlus),
    (builtinMinus, VBuiltin BMinus),
    (builtinMult, VBuiltin BMult),
    (builtinDiv, VBuiltin BDiv),
    (builtinMod, VBuiltin BMod),
    (builtinEq, VBuiltin BEq),
    (builtinLt, VBuiltin BLt)
  ]

defineSymbol, lambdaSymbol, ifSymbol :: SymbolName
defineSymbol = SymbolName "define"
lambdaSymbol = SymbolName "lambda"
ifSymbol = SymbolName "if"
