module SexprtoAST where

import Ast (Ast (..), ConvertResult, ErrorMsg (..), SExpr (..), VarName (..), defineSymbol, ifSymbol, lambdaSymbol, mkError, symbolToVar, varToParam)

-- | Convert an SExpr to an Ast, with error handling
sexprToAst :: SExpr -> ConvertResult
sexprToAst (SInteger n) = Right $ LiteralInt n
sexprToAst (SBool b) = Right $ LiteralBool b
sexprToAst (SSymbol s) = Right $ VariableRef (symbolToVar s)
sexprToAst (SList xs) = convertList xs

-- | Helper to convert SExpr list to Ast
convertList :: [SExpr] -> ConvertResult
convertList [] = Left $ mkError "Empty S-expression list cannot be converted to AST."
convertList (SSymbol sym : rest)
  | sym == defineSymbol = convertDefine rest
  | sym == lambdaSymbol = convertLambda rest
  | sym == ifSymbol = convertIf rest
  | otherwise = convertCall (SSymbol sym) rest
convertList (f : args) = convertCall f args

-- | Convert define form: (define var expr)
convertDefine :: [SExpr] -> ConvertResult
convertDefine [SSymbol var, expr] = do
  astExpr <- sexprToAst expr
  return $ Define (symbolToVar var) astExpr
convertDefine _ = Left $ mkError "Malformed define: expected (define var expr)"

-- | Convert lambda form: (lambda (params...) body)
convertLambda :: [SExpr] -> ConvertResult
convertLambda [SList params, body] = do
  paramNames <- mapM expectSymbol params
  astBody <- sexprToAst body
  return $ Lambda (map symbolToParam paramNames) astBody
  where
    expectSymbol (SSymbol s) = Right s
    expectSymbol _ = Left $ mkError "Lambda parameters must be symbols."
    symbolToParam = varToParam . symbolToVar
convertLambda _ = Left $ mkError "Malformed lambda: expected (lambda (params) body)"

-- | Convert if form: (if cond then else)
convertIf :: [SExpr] -> ConvertResult
convertIf [cond, thenExpr, elseExpr] = do
  astCond <- sexprToAst cond
  astThen <- sexprToAst thenExpr
  astElse <- sexprToAst elseExpr
  return $ If astCond astThen astElse
convertIf _ = Left $ mkError "Malformed if: expected (if cond then else)"

-- | Convert function call: (f arg1 arg2 ...)
convertCall :: SExpr -> [SExpr] -> ConvertResult
convertCall f args = do
  astF <- sexprToAst f
  astArgs <- mapM sexprToAst args
  return $ Call astF astArgs

-- | Helper to convert a symbol to a variable name
symbolToVarName :: SExpr -> Either ErrorMsg VarName
symbolToVarName (SSymbol s) = Right (symbolToVar s)
symbolToVarName _ = Left $ mkError "Expected symbol for variable name."
