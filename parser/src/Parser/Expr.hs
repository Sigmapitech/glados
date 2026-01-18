module Parser.Expr where

import AST.Types.AST
  ( Expr (..),
  )
import AST.Types.Common (FuncName (..), Located (..), VarName (..), getSpan, unLocated)
import AST.Types.Operator (binaryOpPrecedence)
import AST.Types.Type (Type (..))
import Parser.Literal (parseLiteral)
import Parser.Operator (parseBinaryOp, parseUnaryOp)
import Parser.Type (parsePrimitiveType)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchKeyword,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseExprLiteral :: TokenParser (Located (Expr ann))
parseExprLiteral = do
  lit <- parseLiteral parseExpr
  return $ Located (getSpan lit) (ExprLiteral (unLocated lit))

parseExprVar :: TokenParser (Located (Expr ann))
parseExprVar = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (ExprVar (Located span (VarName name)))

parseExprCall :: TokenParser (Located (Expr ann))
parseExprCall = do
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  _ <- matchSymbol "("
  args <- MP.sepBy parseExpr (matchSymbol ",")
  Located endSpan _ <- matchSymbol ")"
  return $ Located (nameSpan <> endSpan) (ExprCall (Located nameSpan (FuncName name)) args)

parseExprIndex :: TokenParser (Located (Expr ann))
parseExprIndex = do
  expr <- (parseExprVar MP.<|> parseExprParen) :: TokenParser (Located (Expr ann))
  indexExpr <- MP.many $ do
    Located startSpan _ <- matchSymbol "["
    i <- parseExpr
    Located endSpan _ <- matchSymbol "]"
    return (Located startSpan (), i, Located endSpan ())
  return $ foldl addIndex expr indexExpr
  where
    addIndex :: Located (Expr ann) -> (Located (), Located (Expr ann), Located ()) -> Located (Expr ann)
    addIndex expr (Located startSpan (), i, Located endSpan ()) =
      let combinedSpan = getSpan expr <> startSpan <> getSpan i <> endSpan
       in Located combinedSpan (ExprIndex expr i)

parseExprCast :: TokenParser (Located (Expr ann))
parseExprCast = do
  Located startSpan primitiv <- parsePrimitiveType
  _ <- matchSymbol "("
  expr <- parseExpr
  Located endSpan _ <- matchSymbol ")"
  return $ Located (startSpan <> endSpan) (ExprCast expr (Located startSpan (TypePrimitive primitiv)))

parseExprParen :: TokenParser (Located (Expr ann))
parseExprParen = do
  Located startSpan _ <- matchSymbol "("
  expr <- parseExpr
  Located endSpan _ <- matchSymbol ")"
  return $ Located (startSpan <> endSpan) (ExprParen expr)

parseExprMust :: TokenParser (Located (Expr ann))
parseExprMust = do
  Located startSpan _ <- matchKeyword "must"
  expr <- parseExpr
  return $ Located (startSpan <> getSpan expr) (ExprMust expr)

parsePrimary :: TokenParser (Located (Expr ann))
parsePrimary =
  MP.choice
    [ parseExprLiteral,
      MP.try parseExprCall,
      parseExprCast,
      parseExprParen,
      MP.try parseExprIndex,
      parseExprVar
    ]

parseUnary :: TokenParser (Located (Expr ann))
parseUnary =
  MP.choice
    [ parseExprMust,
      do
        op <- parseUnaryOp
        expr <- parseUnary
        let combinedSpan = getSpan op <> getSpan expr
        return $ Located combinedSpan (ExprUnary (unLocated op) expr),
      parsePrimary
    ]

parseBinary :: Int -> TokenParser (Located (Expr ann))
parseBinary minPrec = do
  left <- parseUnary
  parseBinaryRHS minPrec left
  where
    parseBinaryRHS :: Int -> Located (Expr ann) -> TokenParser (Located (Expr ann))
    parseBinaryRHS minPrec' left = do
      maybeOp <- MP.optional (MP.try parseBinaryOp)
      case maybeOp of
        Nothing -> return left
        Just op -> do
          let prec = binaryOpPrecedence (unLocated op)
          if prec < minPrec'
            then return left
            else do
              right <- parseBinary (prec + 1) -- Left-associative
              let combinedSpan = getSpan left <> getSpan op <> getSpan right
              let newExpr = Located combinedSpan (ExprBinary (unLocated op) left right)
              parseBinaryRHS minPrec' newExpr

parseExpr :: TokenParser (Located (Expr ann))
parseExpr = parseBinary 0
