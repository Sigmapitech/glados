{-# LANGUAGE LambdaCase #-}

-- | Evaluator for the Lisp interpreter.
-- This module implements the evaluation logic for AST nodes,
-- handling variable lookups, function calls, conditionals, and built-in operations.
module Evaluator
  ( eval,
    evalWithEnv,
    initialEnv,
  )
where

import Ast
import Control.Monad.State (get, gets, put)

initialEnv :: Environment
initialEnv = emptyEnv

evalWithEnv :: Ast -> Environment -> EvalResult Value
evalWithEnv ast = runEvaluator (eval ast)

eval :: Ast -> Evaluator Value
eval = \case
  LiteralInt n -> return $ VInt n
  LiteralBool b -> return $ VBool b
  Variable name -> evalVariable name
  Define name expr -> evalDefine name expr
  Lambda params body -> evalLambda params body
  Call func args -> evalCall func args
  If cond thenExpr elseExpr -> evalIf cond thenExpr elseExpr

evalVariable :: VarName -> Evaluator Value
evalVariable name = do
  env <- get
  case lookupEnv name env of
    Just val -> return val
    Nothing -> throwEvalError $ "Undefined variable: " ++ unVarName name

evalDefine :: VarName -> Ast -> Evaluator Value
evalDefine name expr = do
  env <- get
  case expr of
    Lambda params body -> do
      let recursiveEnv = extendEnv name recursiveValue env
          recursiveValue = VFunction params body recursiveEnv
      put $ extendEnv name recursiveValue env
      return VUnit
    _ -> do
      value <- eval expr
      put $ extendEnv name value env
      return VUnit

evalLambda :: [ParamName] -> Ast -> Evaluator Value
evalLambda params body = do
  gets (VFunction params body)

evalCall :: Ast -> [Ast] -> Evaluator Value
evalCall funcExpr argExprs = do
  case funcExpr of
    Variable name | isBuiltin name -> do
      args <- mapM eval argExprs
      evalBuiltin name args
    _ -> do
      func <- eval funcExpr
      case func of
        VFunction params body closureEnv -> do
          args <- mapM eval argExprs
          if length params /= length args
            then throwEvalError $ "Function expects " ++ show (length params) ++ " arguments, got " ++ show (length args)
            else do
              currentEnv <- get
              let newEnv = foldr (uncurry extendEnv) closureEnv (zip (map paramToVar params) args)
              put newEnv
              result <- eval body
              put currentEnv
              return result
        _ -> throwEvalError "Attempted to call a non-function value"

evalIf :: Ast -> Ast -> Ast -> Evaluator Value
evalIf condExpr thenExpr elseExpr = do
  cond <- eval condExpr
  case cond of
    VBool True -> eval thenExpr
    VBool False -> eval elseExpr
    _ -> throwEvalError "Condition in 'if' must evaluate to a boolean"

evalBuiltin :: VarName -> [Value] -> Evaluator Value
evalBuiltin name args
  | name == builtinPlus = evalArithmetic (+) args
  | name == builtinMinus = evalArithmetic (-) args
  | name == builtinMult = evalArithmetic (*) args
  | name == builtinDiv = evalDivision args
  | name == builtinMod = evalModulo args
  | name == builtinEq = evalEquality args
  | name == builtinLt = evalLessThan args
  | otherwise = throwEvalError $ "Unknown built-in function: " ++ unVarName name

evalArithmetic :: (Integer -> Integer -> Integer) -> [Value] -> Evaluator Value
evalArithmetic op args = do
  let extractInts [] = return []
      extractInts (VInt n : rest) = (n :) <$> extractInts rest
      extractInts _ = throwEvalError "Arithmetic operation requires integer arguments"
  nums <- extractInts args
  case nums of
    [] -> throwEvalError "Arithmetic operation requires at least one argument"
    [x] -> return $ VInt x
    (x : xs) -> return $ VInt $ foldl op x xs

evalDivision :: [Value] -> Evaluator Value
evalDivision [VInt a, VInt b]
  | b == 0 = throwEvalError "Division by zero"
  | otherwise = return $ VInt (a `div` b)
evalDivision _ = throwEvalError "Division requires exactly two integer arguments"

evalModulo :: [Value] -> Evaluator Value
evalModulo [VInt a, VInt b]
  | b == 0 = throwEvalError "Modulo by zero"
  | otherwise = return $ VInt (a `mod` b)
evalModulo _ = throwEvalError "Modulo requires exactly two integer arguments"

evalEquality :: [Value] -> Evaluator Value
evalEquality [VInt a, VInt b] = return $ VBool (a == b)
evalEquality [VBool a, VBool b] = return $ VBool (a == b)
evalEquality _ = throwEvalError "Equality comparison requires exactly two arguments of the same type"

evalLessThan :: [Value] -> Evaluator Value
evalLessThan [VInt a, VInt b] = return $ VBool (a < b)
evalLessThan _ = throwEvalError "Less-than comparison requires exactly two integer arguments"
