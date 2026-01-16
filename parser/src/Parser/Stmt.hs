module Parser.Stmt where

import AST.Types.AST
  ( Block (Block),
    Stmt (..),
  )
import AST.Types.Common (Located (..), VarName (..))
import Parser.Expr (parseExpr)
import Parser.LValue (parseLValue)
import Parser.Operator (parseAssignOp)
import Parser.Type (parseQualifiedType)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchKeyword,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseBlock :: TokenParser (Located (Block ann))
parseBlock = do
  Located startSpan _ <- matchSymbol "{"
  stmts <- MP.sepBy parseStmt (matchSymbol ";")
  Located endSpan _ <- matchSymbol "}"
  let combinedSpan = startSpan <> endSpan
  return $ Located combinedSpan (Block combinedSpan stmts)

parseStmtAssign :: TokenParser (Located (Stmt ann))
parseStmtAssign = do
  Located lvalueSpan lvalue <- parseLValue
  Located opSpan assignOp <- parseAssignOp
  Located exprSpan expr <- parseExpr
  let combinedSpan = lvalueSpan <> opSpan <> exprSpan
  return $ Located combinedSpan (StmtAssign (Located lvalueSpan lvalue) assignOp (Located exprSpan expr))

parseStmtVarDecl :: TokenParser (Located (Stmt ann))
parseStmtVarDecl = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  Located colonSpan _ <- matchSymbol ":"
  Located typeSpan qtype <- parseQualifiedType
  assignment <- MP.optional $ do
    Located assignSpan _ <- matchSymbol "="
    Located exprSpan expr <- parseExpr
    return (Located assignSpan (), Located exprSpan expr)
  case assignment of
    Just (Located assignSpan _, Located exprSpan expr) ->
      let combinedSpan = span <> colonSpan <> typeSpan <> assignSpan <> exprSpan
       in return $ Located combinedSpan (StmtVarDecl (Located span (VarName name)) (Located typeSpan qtype) (Just (Located exprSpan expr)))
    Nothing ->
      let combinedSpan = span <> colonSpan <> typeSpan
       in return $ Located combinedSpan (StmtVarDecl (Located span (VarName name)) (Located typeSpan qtype) Nothing)

parseStmtExpr :: TokenParser (Located (Stmt ann))
parseStmtExpr = do
  Located exprSpan expr <- parseExpr
  return $ Located exprSpan (StmtExpr (Located exprSpan expr))

parseStmtReturn :: TokenParser (Located (Stmt ann))
parseStmtReturn = do
  Located span _ <- matchKeyword "return"
  maybeExpr <- MP.optional parseExpr
  case maybeExpr of
    Just (Located exprSpan expr) ->
      let combinedSpan = span <> exprSpan
       in return $ Located combinedSpan (StmtReturn (Just (Located exprSpan expr)))
    Nothing ->
      return $ Located span (StmtReturn Nothing)

parseStmtBreak :: TokenParser (Located (Stmt ann))
parseStmtBreak = do
  Located span _ <- matchKeyword "break"
  return $ Located span StmtBreak

parseStmtContinue :: TokenParser (Located (Stmt ann))
parseStmtContinue = do
  Located span _ <- matchKeyword "continue"
  return $ Located span StmtContinue

parseStmtBlock :: TokenParser (Located (Stmt ann))
parseStmtBlock = do
  Located span block <- parseBlock
  return $ Located span (StmtBlock block)

parseStmt :: TokenParser (Located (Stmt ann))
parseStmt =
  MP.choice
    [ MP.try parseStmtVarDecl,
      MP.try parseStmtAssign,
      parseStmtExpr,
      parseStmtReturn,
      parseStmtBreak,
      parseStmtContinue,
      parseStmtBlock
    ]
