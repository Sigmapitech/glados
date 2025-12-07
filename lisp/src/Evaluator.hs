{-# LANGUAGE LambdaCase #-}

-- | Evaluator for the Lisp interpreter.
-- This module implements the evaluation logic for AST nodes,
-- handling VariableRef lookups, function calls, conditionals, and built-in operations.
module Evaluator (eval, evalFrom, evalMany, evalManyFrom, evalToValue, evalManyToValue) where

import Ast
import Control.Monad.Except (runExceptT)
import Control.Monad.State (get, modify, put, runState)
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import qualified Data.List.NonEmpty as NE

createEvaluator :: Ast -> Evaluator
createEvaluator = \case
  LiteralInt number -> return $ VInt number
  LiteralBool bool -> return $ VBool bool
  Lambda params body -> return $ VProcedure Nothing params body
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
        Nothing -> throwEvalError $ "variable " ++ unVarName name ++ " is not bound."

    defineEvaluator :: VarName -> Ast -> Evaluator
    defineEvaluator name expr =
      (createEvaluator expr >>= declareInEnv)
        >> return VUnit
      where
        declareInEnv = (modify . extendEnv name) . ensureNamedProcedure

        ensureNamedProcedure = \case
          (VProcedure Nothing param body) -> (VProcedure (Just name) param body)
          other -> other

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
    applyFuncEvaluator (VProcedure _ params body) args
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
evalFrom env ast =
  let (result, env') = env & runEvaluator (createEvaluator ast)
   in (NE.singleton result, env')

-- | Evaluate multiple ASTs from a given environment, return all results
--
-- This function evaluates each AST and collects ALL results (both successes and errors).
--
-- Key insight: By using runExceptT before mapM, we detach the ExceptT layer,
-- converting errors from control flow into data (Either values). This prevents
-- short-circuiting on errors, allowing all evaluations to proceed.
--
-- How it works:
--
-- >>> mapM (runExceptT . createEvaluator) [ast1, ast2, ast3] :: NonEmpty Ast
-- -- Is equivalent to:
-- do
--   result1 <- runExceptT (createEvaluator ast1)  -- State Environment ValueResult
--   result2 <- runExceptT (createEvaluator ast2)  -- State Environment ValueResult
--   result3 <- runExceptT (createEvaluator ast3)  -- State Environment ValueResult
--   return (result1 :| [result2, result3])        -- State Environment (NonEmpty ValueResult)
--
-- This allows MapM to only thread States.
evalManyFrom :: Environment -> [Ast] -> EvalResult
evalManyFrom env [] = (NE.singleton (Left "No expression to evaluate"), env)
evalManyFrom env asts = env & runState (mapM (runExceptT . createEvaluator) $ NE.fromList asts)

eval :: Ast -> EvalResult
eval = evalFrom initialEnv

evalMany :: [Ast] -> EvalResult
evalMany = evalManyFrom initialEnv

-- | Get only the value, discarding the final environment
evalToValue :: Ast -> Result RuntimeValue
evalToValue ast = NE.head $ fst $ eval ast

-- | Discard the final environment
evalManyToValue :: [Ast] -> ValueResult
evalManyToValue asts = fst $ evalMany asts
