-- | Builtin definitions for the Lisp interpreter.
-- This module defines the functions, operators and values that are
-- handled by the interpreter.
module Builtins where

import Ast
import Control.Monad.Except (throwError)
import Data.Bits (Bits (..))

-- | Initial environment
initialEnv :: Environment
initialEnv =
  [ (builtinTrue, VBool True),
    (builtinFalse, VBool False)
    -- +inf & -inf?
  ]

-- | Apply a built-in function
applyBuiltin :: VarName -> [Value] -> Evaluator Value
applyBuiltin name args
  -- Arithmetic operators
  | name == builtinPlus = numericBinop (+) args
  | name == builtinMinus = numericBinop (-) args
  | name == builtinMult = numericBinop (*) args
  | name == builtinDiv = numericBinop div args
  | name == builtinMod = numericBinop mod args
  | name == builtinIncrement = numericUnop (+ 1) args
  | name == builtinDecrement = numericUnop (subtract 1) args
  -- Comparison operators
  | name == builtinEq = comparisonBinop (==) args
  | name == builtinNeq = comparisonBinop (/=) args
  | name == builtinLt = comparisonBinop (<) args
  | name == builtinGt = comparisonBinop (>) args
  | name == builtinLte = comparisonBinop (<=) args
  | name == builtinGte = comparisonBinop (>=) args
  -- Bitwise operators
  | name == builtinBitAnd = bitwiseBinop (.&.) args
  | name == builtinBitOr = bitwiseBinop (.|.) args
  | name == builtinBitXor = bitwiseBinop xor args
  | name == builtinBitComplement = complementOp args
  | name == builtinBitShiftLeft = bitShiftOp shiftL args
  | name == builtinBitShiftRight = bitShiftOp shiftR args
  -- Unknown
  | otherwise = throwEvalError $ "Unknown function: " ++ unVarName name

-- | Helper for numeric binary operations
numericBinop :: (Integer -> Integer -> Integer) -> [Value] -> Evaluator Value
numericBinop _ [] = throwEvalError "Numeric operation requires at least one argument"
numericBinop op (x : xs) = do
  firstVal <- extractInt x
  result <- foldM applyOp firstVal xs
  return $ VInt result
  where
    applyOp acc val = do
      n <- extractInt val
      return $ acc `op` n
    foldM _ acc [] = return acc
    foldM f acc (y : ys) = do
      acc' <- f acc y
      foldM f acc' ys

-- | Helper for comparison binary operations (only works on two arguments)
comparisonBinop :: (Integer -> Integer -> Bool) -> [Value] -> Evaluator Value
comparisonBinop op [VInt x, VInt y] = return $ VBool $ x `op` y
comparisonBinop _ [VInt _, _] = throwEvalError "Type mismatch: expected integer"
comparisonBinop _ [_, _] = throwEvalError "Type mismatch: expected integer"
comparisonBinop _ args = throwEvalError $ "Comparison requires exactly 2 arguments, got " ++ show (length args)

-- | Helper for bitwise binary operations
bitwiseBinop :: (Integer -> Integer -> Integer) -> [Value] -> Evaluator Value
bitwiseBinop _ [] = throwEvalError "Bitwise operation requires at least one argument"
bitwiseBinop op (x : xs) = do
  firstVal <- extractInt x
  result <- foldM applyOp firstVal xs
  return $ VInt result
  where
    applyOp acc val = do
      n <- extractInt val
      return $ acc `op` n
    foldM _ acc [] = return acc
    foldM f acc (y : ys) = do
      acc' <- f acc y
      foldM f acc' ys

-- | Helper for numeric unary operations
numericUnop :: (Integer -> Integer) -> [Value] -> Evaluator Value
numericUnop op [VInt x] = return $ VInt $ op x
numericUnop _ [_] = throwEvalError "Type mismatch: expected integer"
numericUnop _ args = throwEvalError $ "Unary operation requires exactly 1 argument, got " ++ show (length args)

-- | Complement operation
complementOp :: [Value] -> Evaluator Value
complementOp [VInt x] = return $ VInt $ complement x
complementOp [VBool b] = return $ VBool $ not b
complementOp [_] = throwEvalError "Type mismatch: expected integer or boolean"
complementOp args = throwEvalError $ "Complement requires exactly 1 argument, got " ++ show (length args)

-- | Helper for bit shift operations
bitShiftOp :: (Integer -> Int -> Integer) -> [Value] -> Evaluator Value
bitShiftOp op [VInt x, VInt y] = return $ VInt $ x `op` fromInteger y
bitShiftOp _ [VInt _, _] = throwEvalError "Type mismatch: expected integer for shift amount"
bitShiftOp _ [_, _] = throwEvalError "Type mismatch: expected integer"
bitShiftOp _ args = throwEvalError $ "Bit shift requires exactly 2 arguments, got " ++ show (length args)

-- | Extract an integer from a value, or throw an error
extractInt :: Value -> Evaluator Integer
extractInt (VInt n) = return n
extractInt _ = throwEvalError "Type mismatch: expected integer"
