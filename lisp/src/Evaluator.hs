{-# LANGUAGE LambdaCase #-}

-- | Evaluator for the Lisp interpreter.
-- This module implements the evaluation logic for AST nodes,
-- handling VariableRef lookups, function calls, conditionals, and built-in operations.
module Evaluator (eval, evalFrom, evalMany, evalManyFrom, evalToValue) where

import Ast
import Control.Monad.State (get, modify, put)
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))

createEvaluator :: Ast -> Evaluator
createEvaluator = \case
  LiteralInt number -> return $ VInt number
  LiteralBool bool -> return $ VBool bool
  Lambda params body -> return $ VProcedure params body
  VariableRef name -> variableRefEvaluator name
  Define name expr -> defineEvaluator name expr
  If condExpr thenExpr elseExpr -> ifEvaluator condExpr thenExpr elseExpr
  Call funcExpr argExprs -> callEvaluator funcExpr argExprs
  where
    variableRefEvaluator :: VarName -> Evaluator
    variableRefEvaluator name = do
      env <- get
      case lookupEnv name env of
        Just val -> return val
        Nothing -> throwEvalError $ "Unbound variable: " ++ unVarName name

    defineEvaluator :: VarName -> Ast -> Evaluator
    defineEvaluator name expr = do
      value <- createEvaluator expr
      modify (extendEnv name value)
      return VUnit

    ifEvaluator :: Ast -> Ast -> Ast -> Evaluator
    ifEvaluator condExpr thenExpr elseExpr = do
      cond <- createEvaluator condExpr
      case cond of
        VBool True -> createEvaluator thenExpr
        VBool False -> createEvaluator elseExpr
        _ -> throwEvalError "Condition in 'if' must evaluate to a boolean"

    -- Buitin handling was defer to the function application
    -- this allow the builting functions to show in the environment
    callEvaluator :: Ast -> [Ast] -> Evaluator
    callEvaluator funcExpr argExprs = do
      func <- createEvaluator funcExpr
      args <- mapM createEvaluator argExprs
      applyFuncEvaluator func args

    applyFuncEvaluator :: RuntimeValue -> [RuntimeValue] -> Evaluator
    applyFuncEvaluator (VBuiltin op) args = getBuiltinEvaluator op args
    applyFuncEvaluator (VProcedure params body) args
      | length params /= length args =
          throwEvalError $ "Expected " ++ show (length params) ++ " arguments, got" ++ show (length args)
      | otherwise = do
          currentEnv <- get
          let paramNames = map paramToVar params
              paramBindings = zip paramNames args
              evalEnv = foldl' (flip (uncurry extendEnv)) currentEnv paramBindings
          put evalEnv
          result <- createEvaluator body
          put currentEnv
          return result
    applyFuncEvaluator _ _ = throwEvalError "Attempted to call a non-function value"

getBuiltinEvaluator :: BuitinOp -> [RuntimeValue] -> Evaluator
getBuiltinEvaluator op args
  | length args /= 2 = throwEvalError $ show op ++ " expects exactly 2 arguments"
  | otherwise = case (op, args) of
      (BPlus, [VInt a, VInt b]) -> return $ VInt (a + b)
      (BMinus, [VInt a, VInt b]) -> return $ VInt (a - b)
      (BMult, [VInt a, VInt b]) -> return $ VInt (a * b)
      (BDiv, [VInt a, VInt b])
        | b == 0 -> throwEvalError "Unexpected division by zero"
        | otherwise -> return $ VInt (a `div` b)
      (BMod, [VInt a, VInt b])
        | b == 0 -> throwEvalError "Unexpect modulo by zero"
        | otherwise -> return $ VInt (a `mod` b)
      (BEq, [VInt a, VInt b]) -> return $ VBool (a == b)
      (BEq, [VBool a, VBool b]) -> return $ VBool (a == b)
      (BLt, [VInt a, VInt b]) -> return $ VBool (a < b)
      (_, _) -> throwEvalError "Unexpected type mismatch"

evalFrom :: Environment -> Ast -> EvalResult
evalFrom env ast = env & runEvaluator (createEvaluator ast)

-- | Evaluate multiple ASTs from a given environment, return last value
--
-- >>>
-- -- mapM in a State monad automatically threads state:
-- mapM createEvaluator [ast1, ast2]
-- -- is equivalent to:
-- do
--   v1 <- createEvaluator ast1
--   v2 <- createEvaluator ast2
--   return [v1, v2]
evalManyFrom :: Environment -> [Ast] -> EvalResult
evalManyFrom env [] = env & runEvaluator (throwEvalError "No expression to evaluate")
evalManyFrom env asts = env & runEvaluator (last <$> mapM createEvaluator asts)

eval :: Ast -> EvalResult
eval = evalFrom initialEnv

evalMany :: [Ast] -> EvalResult
evalMany = evalManyFrom initialEnv

-- | Get only the value, discarding the final environment
evalToValue :: [Ast] -> ValueResult
evalToValue asts = fst $ evalMany asts
