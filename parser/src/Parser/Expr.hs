module Parser.Expr where

import AST.Types.AST
  ( Expr (..),
  )
import AST.Types.Common (FieldName (..), FuncName (..), Located (..), TypeName (..), VarName (..))
import AST.Types.Operator (binaryOpPrecedence)
import qualified Data.Set as Set
import Parser.Literal (parseLiteral)
import Parser.Operator (parseBinaryOp, parseUnaryOp)
import Parser.Type (parseType, parseTypeNamed)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseExprVar :: TokenParser (Located (Expr ann))
parseExprVar = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (ExprVar (Located span (VarName name)))

parseFieldInit :: TokenParser (Located FieldName, Located (Expr ann))
parseFieldInit = do
  Located fieldSpan (TokIdentifier fieldName) <- MP.satisfy isIdentifier
  _ <- matchSymbol "="
  expr <- parseExpr
  return (Located fieldSpan (FieldName fieldName), expr)

parseExprArrayInit :: TokenParser (Located (Expr ann))
parseExprArrayInit = do
  locatedType <- parseType
  _ <- matchSymbol "{"
  exprs <- MP.sepBy parseExpr (matchSymbol ",")
  Located endSpan _ <- matchSymbol "}"
  let Located typeSpan _ = locatedType
  return $ Located (typeSpan <> endSpan) (ExprArrayInit locatedType exprs)

parseExprPrimary :: TokenParser (Located (Expr ann))
parseExprPrimary =
  MP.choice
    [ fmap ExprLiteral <$> parseLiteral parseExpr,
      do
        Located startSpan _ <- matchSymbol "("
        expr <- parseExpr
        Located endSpan _ <- matchSymbol ")"
        return $ Located (startSpan <> endSpan) (ExprParen expr),
      MP.try $ do
        Located typeSpan (TypeName typeName) <- parseTypeNamed
        _ <- matchSymbol "{"
        fields <- MP.sepBy parseFieldInit (matchSymbol ",")
        Located endSpan _ <- matchSymbol "}"
        return $ Located (typeSpan <> endSpan) (ExprStructInit (Located typeSpan (TypeName typeName)) fields),
      MP.try parseExprArrayInit,
      parseExprVar
    ]

parseExprPostfix :: TokenParser (Located (Expr ann))
parseExprPostfix = do
  base <- parseExprPrimary
  parsePostfixOps base
  where
    parsePostfixOps expr = do
      maybeOp <-
        MP.optional $
          MP.choice
            [ do
                _ <- matchSymbol "("
                args <- MP.sepBy parseExpr (matchSymbol ",")
                Located endSpan _ <- matchSymbol ")"
                case expr of
                  Located exprSpan (ExprVar (Located _ (VarName name))) ->
                    return $ Located (exprSpan <> endSpan) (ExprCall (Located exprSpan (FuncName name)) args)
                  _ -> MP.failure Nothing Set.empty,
              do
                _ <- matchSymbol "["
                index <- parseExpr
                Located endSpan _ <- matchSymbol "]"
                let Located exprSpan _ = expr
                return $ Located (exprSpan <> endSpan) (ExprIndex expr index),
              do
                _ <- matchSymbol "."
                Located fieldSpan (TokIdentifier fieldName) <- MP.satisfy isIdentifier
                let Located exprSpan _ = expr
                return $ Located (exprSpan <> fieldSpan) (ExprField expr (Located fieldSpan (FieldName fieldName))),
              do
                Located trySpan _ <- matchSymbol "?"
                let Located exprSpan _ = expr
                return $ Located (exprSpan <> trySpan) (ExprTry expr),
              do
                Located mustSpan _ <- matchSymbol "!"
                let Located exprSpan _ = expr
                return $ Located (exprSpan <> mustSpan) (ExprMust expr)
            ]
      case maybeOp of
        Just newExpr -> parsePostfixOps newExpr
        Nothing -> return expr

parseExprUnary :: TokenParser (Located (Expr ann))
parseExprUnary = do
  maybeOp <- MP.optional parseUnaryOp
  case maybeOp of
    Just (Located opSpan op) -> do
      expr <- parseExprUnary
      let Located exprSpan _ = expr
      return $ Located (opSpan <> exprSpan) (ExprUnary op expr)
    Nothing -> parseExprPostfix

parseExprBinary :: TokenParser (Located (Expr ann))
parseExprBinary = do
  left <- parseExprUnary
  parseExprBinaryRHS 0 left
  where
    parseExprBinaryRHS :: Int -> Located (Expr ann) -> TokenParser (Located (Expr ann))
    parseExprBinaryRHS minPrec left = do
      maybeOp <- MP.optional parseBinaryOp
      case maybeOp of
        Nothing -> return left
        Just (Located _ op) -> do
          let prec = binaryOpPrecedence op
          if prec < minPrec
            then return left
            else do
              right <- parseExprUnary
              right' <- parseExprBinaryRHS (prec + 1) right
              let Located leftSpan _ = left
                  Located rightSpan _ = right'
                  expr = Located (leftSpan <> rightSpan) (ExprBinary op left right')
              parseExprBinaryRHS minPrec expr

parseExpr :: TokenParser (Located (Expr ann))
parseExpr = parseExprBinary
