{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Abstract Syntax Tree definitions for the Lisp interpreter.
-- This module defines the core data types for representing parsed S-expressions,
-- AST nodes, runtime values, and environments.
module AST where

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

data AST
  = LiteralInt Integer
  | LiteralBool Bool
  | VariableRef VarName
  | -- | Variable definition (Function or value)
    Define VarName AST
  | Lambda [ParamName] AST
  | Call AST [AST]
  | If {ifCond :: AST, ifThen :: AST, ifElse :: AST}
  deriving (Eq)

instance Show AST where
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
  | -- we need to store the name of the procedure when it is created with
    -- (define (name) ...)
    -- see: https://github.com/cisco/ChezScheme/blob/v10.3.0/s/print.ss#L776-L785C43
    VProcedure (Maybe VarName) [ParamName] AST
  | VBuiltin BuitinOp
  | VUnit -- Represents 'void' or 'no value': "() <- unit"
  deriving (Eq)

instance Show RuntimeValue where
  show (VInt n) = show n
  show (VBool b) = if b then "#t" else "#f"
  show (VProcedure mName _ _) = case mName of
    (Just name) -> "#\\<procedure " ++ unVarName name ++ "\\>"
    Nothing -> "#\\<procedure\\>"
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

type ConvertResult = Result AST

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
  | BNeq
  | BLt
  | BGt
  | BLte
  | BGte
  | BBitAnd
  | BBitOr
  | BBitXor
  | BBitComplement
  | BShiftLeft
  | BShiftRight
  | BInc
  | BDec
  deriving (Eq)

instance Show BuitinOp where
  -- match scheme builtin names
  -- see: https://github.com/cisco/ChezScheme/blob/v10.3.0/csug/tspl4/tspl.idx
  -- (exported symbol include \scheme{name})
  show BPlus = "+"
  show BMinus = "-"
  show BMult = "*"
  show BDiv = "div"
  show BMod = "mod"
  show BEq = "eq?"
  show BNeq = "neq?"
  show BLt = "<"
  show BGt = ">"
  show BLte = "<="
  show BGte = ">="
  show BBitAnd = "bitwise-and"
  show BBitOr = "bitwise-or"
  show BBitXor = "bitwise-xor"
  show BBitComplement = "bitwise-not"
  show BShiftLeft = "bitwise-arithmetic-shift-left"
  show BShiftRight = "bitwise-arithmetic-shift-right"
  show BInc = "add1"
  show BDec = "sub1"

builtinPlus, builtinMinus, builtinMult :: VarName
builtinPlus = VarName "+"
builtinMinus = VarName "-"
builtinMult = VarName "*"

builtinDiv, builtinMod :: VarName
builtinDiv = VarName "div"
builtinMod = VarName "mod"

builtinEq, builtinNeq :: VarName
builtinEq = VarName "eq?"
builtinNeq = VarName "neq?"

builtinLt, builtinGt, builtinLte, builtinGte :: VarName
builtinLt = VarName "<"
builtinGt = VarName ">"
builtinLte = VarName "<="
builtinGte = VarName ">="

builtinBitAnd, builtinBitOr, builtinBitXor, builtinBitComplement :: VarName
builtinBitAnd = VarName "bitwise-and"
builtinBitOr = VarName "bitwise-or"
builtinBitXor = VarName "bitwise-xor"
builtinBitComplement = VarName "bitwise-not"

builtinBitShiftLeft, builtinBitShiftRight :: VarName
builtinBitShiftLeft = VarName "bitwise-arithmetic-shift-left"
builtinBitShiftRight = VarName "bitwise-arithmetic-shift-right"

-- | Built-in decrement/increment unary operators
builtinIncrement, builtinDecrement :: VarName
builtinIncrement = VarName "add1"
builtinDecrement = VarName "sub1"

allBuitinName :: [VarName]
allBuitinName =
  [ builtinPlus,
    builtinMinus,
    builtinMult,
    builtinDiv,
    builtinMod,
    builtinEq,
    builtinNeq,
    builtinLt,
    builtinGt,
    builtinLte,
    builtinGte,
    builtinBitAnd,
    builtinBitOr,
    builtinBitXor,
    builtinBitComplement,
    builtinBitShiftLeft,
    builtinBitShiftRight,
    builtinIncrement,
    builtinDecrement
  ]

isBuiltin :: VarName -> Bool
isBuiltin name = name `elem` allBuitinName

isVoid :: RuntimeValue -> Bool
isVoid VUnit = True
isVoid _ = False

initialEnv :: Environment
initialEnv =
  [ (builtinPlus, VBuiltin BPlus),
    (builtinMinus, VBuiltin BMinus),
    (builtinMult, VBuiltin BMult),
    (builtinDiv, VBuiltin BDiv),
    (builtinMod, VBuiltin BMod),
    (builtinEq, VBuiltin BEq),
    (builtinNeq, VBuiltin BNeq),
    (builtinLt, VBuiltin BLt),
    (builtinGt, VBuiltin BGt),
    (builtinLte, VBuiltin BLte),
    (builtinGte, VBuiltin BGte),
    (builtinBitAnd, VBuiltin BBitAnd),
    (builtinBitOr, VBuiltin BBitOr),
    (builtinBitXor, VBuiltin BBitXor),
    (builtinBitComplement, VBuiltin BBitComplement),
    (builtinBitShiftLeft, VBuiltin BShiftLeft),
    (builtinBitShiftRight, VBuiltin BShiftRight),
    (builtinIncrement, VBuiltin BInc),
    (builtinDecrement, VBuiltin BDec)
  ]

defineSymbol, lambdaSymbol, ifSymbol :: SymbolName
defineSymbol = SymbolName "define"
lambdaSymbol = SymbolName "lambda"
ifSymbol = SymbolName "if"
