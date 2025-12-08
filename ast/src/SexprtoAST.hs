module SexprtoAST where

import AST (AST (..), ConvertResult, SExpr (..), defineSymbol, ifSymbol, lambdaSymbol, mkError, symbolToVar, varToParam)

-- | Convert an SExpr to an AST, with error handling
sexprToAST :: SExpr -> ConvertResult
sexprToAST (SInteger n) = Right $ LiteralInt n
sexprToAST (SBool b) = Right $ LiteralBool b
sexprToAST (SSymbol s) = Right $ VariableRef (symbolToVar s)
sexprToAST (SList xs) = convertList xs

-- | Helper to convert SExpr list to AST
convertList :: [SExpr] -> ConvertResult
convertList (SSymbol sym : rest)
  | sym == defineSymbol = convertDefine rest
  | sym == lambdaSymbol = convertLambda rest
  | sym == ifSymbol = convertIf rest
  | otherwise = convertCall (SSymbol sym) rest
convertList (f : args) = convertCall f args
convertList [] = Left $ mkError "Empty S-expression list cannot be converted to AST."

-- | Convert define form: (define var expr)
convertDefine :: [SExpr] -> ConvertResult
convertDefine [SSymbol var, expr] = do
  astExpr <- sexprToAST expr
  return $ Define (symbolToVar var) astExpr
-- Function form: (define (name params...) body)
-- Desugar to: (define name (lambda (params...) body))
convertDefine [SList (SSymbol name : params), body] = Define (symbolToVar name) <$> convertLambda [SList params, body]
convertDefine _ = Left $ mkError "Malformed define: expected (define var expr)"

-- | Convert lambda form: (lambda (params...) body)
convertLambda :: [SExpr] -> ConvertResult
convertLambda [SList [], body] = do
  astBody <- sexprToAST body
  return $ Lambda [] astBody
convertLambda [SList params, body] = do
  paramNames <- mapM expectSymbol params
  astBody <- sexprToAST body
  return $ Lambda (map symbolToParam paramNames) astBody
  where
    expectSymbol (SSymbol s) = Right s
    expectSymbol _ = Left $ mkError "Lambda parameters must be symbols."
    symbolToParam = varToParam . symbolToVar
convertLambda _ = Left $ mkError "Malformed lambda: expected (lambda (params) body)"

-- | Convert if form: (if cond then else)
convertIf :: [SExpr] -> ConvertResult
convertIf [cond, thenExpr, elseExpr] = do
  astCond <- sexprToAST cond
  astThen <- sexprToAST thenExpr
  astElse <- sexprToAST elseExpr
  return $ If astCond astThen astElse
convertIf _ = Left $ mkError "Malformed if: expected (if cond then else)"

-- | Convert function call: (f arg1 arg2 ...)
convertCall :: SExpr -> [SExpr] -> ConvertResult
convertCall f [] = do
  astF <- sexprToAST f
  return $ Call astF []
convertCall f args = do
  astF <- sexprToAST f
  astArgs <- mapM sexprToAST args
  return $ Call astF astArgs
