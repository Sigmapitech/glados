{-# LANGUAGE LambdaCase #-}

-- | Evaluator for the Lisp interpreter.
-- This module implements the evaluation logic for AST nodes,
-- handling VariableRef lookups, function calls, conditionals, and built-in operations.
module Evaluator
  ( eval,
    evalWithEnv,
    initialEnv,
  )
where

import Ast
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (State, get, gets, put)

initialEnv :: Environment
initialEnv = emptyEnv

evalWithEnv :: Ast -> Environment -> EvalResult
evalWithEnv ast = runEvaluator (eval ast)

eval :: Ast -> Evaluator
eval = \case
  LiteralInt n -> return $ VInt n
  LiteralBool b -> return $ VBool b
  VariableRef name -> evalVariable name
  Define name expr -> evalDefine name expr
  Lambda params body -> evalLambda params body
  Call func args -> evalCall func args
  If cond thenExpr elseExpr -> evalIf cond thenExpr elseExpr

evalVariable :: VarName -> Evaluator
evalVariable name = do
  env <- get
  case lookupEnv name env of
    Just val -> return val
    Nothing -> throwEvalError $ "Undefined VariableRef: " ++ unVarName name

evalDefine :: VarName -> Ast -> Evaluator
evalDefine name expr = do
  env <- get
  case expr of
    Lambda params body -> do
      let recursiveEnv = extendEnv name recursiveValue env
          recursiveValue = VProcedure params body recursiveEnv
      put $ extendEnv name recursiveValue env
      return VUnit
    _ -> do
      value <- eval expr
      put $ extendEnv name value env
      return VUnit

evalLambda :: [ParamName] -> Ast -> Evaluator
evalLambda params body = do
  gets (VProcedure params body)

evalCall :: Ast -> [Ast] -> Evaluator
evalCall funcExpr argExprs = do
  case funcExpr of
    VariableRef name | isBuiltin name -> do
      args <- mapM eval argExprs
      evalBuiltin name args
    _ -> do
      func <- eval funcExpr
      case func of
        VProcedure params body closureEnv -> do
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

evalIf :: Ast -> Ast -> Ast -> Evaluator
evalIf condExpr thenExpr elseExpr = do
  cond <- eval condExpr
  case cond of
    VBool True -> eval thenExpr
    VBool False -> eval elseExpr
    _ -> throwEvalError "Condition in 'if' must evaluate to a boolean"

evalBuiltin :: VarName -> [RuntimeValue] -> Evaluator
evalBuiltin name args
  | name == builtinPlus = evalArithmetic (+) args
  | name == builtinMinus = evalArithmetic (-) args
  | name == builtinMult = evalArithmetic (*) args
  | name == builtinDiv = evalDivision args
  | name == builtinMod = evalModulo args
  | name == builtinEq = evalEquality args
  | name == builtinLt = evalLessThan args
  | otherwise = throwEvalError $ "Unknown built-in function: " ++ unVarName name

evalArithmetic :: (Integer -> Integer -> Integer) -> [RuntimeValue] -> Evaluator
evalArithmetic op args = do
  nums <- extractInts args
  case nums of
    [] -> throwEvalError "Arithmetic operation requires at least one argument"
    [x] -> return $ VInt x
    (x : xs) -> return $ VInt $ foldl op x xs
  where
    extractInts :: [RuntimeValue] -> ExceptT ErrorMsg (State Environment) [Integer]
    extractInts [] = return []
    extractInts (VInt n : rest) = (n :) <$> extractInts rest
    extractInts _ = throwError $ mkError "Arithmetic operation requires integer arguments"

evalDivision :: [RuntimeValue] -> Evaluator
evalDivision [VInt a, VInt b]
  | b == 0 = throwEvalError "Division by zero"
  | otherwise = return $ VInt (a `div` b)
evalDivision _ = throwEvalError "Division requires exactly two integer arguments"

evalModulo :: [RuntimeValue] -> Evaluator
evalModulo [VInt a, VInt b]
  | b == 0 = throwEvalError "Modulo by zero"
  | otherwise = return $ VInt (a `mod` b)
evalModulo _ = throwEvalError "Modulo requires exactly two integer arguments"

evalEquality :: [RuntimeValue] -> Evaluator
evalEquality [VInt a, VInt b] = return $ VBool (a == b)
evalEquality [VBool a, VBool b] = return $ VBool (a == b)
evalEquality _ = throwEvalError "Equality comparison requires exactly two arguments of the same type"

evalLessThan :: [RuntimeValue] -> Evaluator
evalLessThan [VInt a, VInt b] = return $ VBool (a < b)
evalLessThan _ = throwEvalError "Less-than comparison requires exactly two integer arguments"
