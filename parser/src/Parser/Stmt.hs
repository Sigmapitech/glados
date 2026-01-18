module Parser.Stmt where

import AST.Types.AST
  ( Block (Block),
    Expr (..),
    ForInit (..),
    Stmt (..),
  )
import AST.Types.Common (Located (..), VarName (..), getSpan, SourceSpan(..))
import AST.Types.Literal (IntBase (..), IntLiteral (..), Literal (..))
import AST.Types.Operator (AssignOp (AssignAdd, AssignSub))
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
  stmts <- MP.endBy parseStmt (matchSymbol ";")
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

parseStmtWhile :: TokenParser (Located (Stmt ann))
parseStmtWhile = do
  Located whileSpan _ <- matchKeyword "while"
  Located lpan _ <- matchSymbol "("
  Located condSpan condExpr <- parseExpr
  Located rpan _ <- matchSymbol ")"
  Located bodySpan bodyBlock <- parseBlock
  let combinedSpan = whileSpan <> lpan <> condSpan <> rpan <> bodySpan
  return $ Located combinedSpan (StmtWhile (Located condSpan condExpr) bodyBlock)

parseForInit :: TokenParser (ForInit ann)
parseForInit = do
  MP.choice
    [ do
        Located _ var <- parseStmtVarDecl
        case var of
          StmtVarDecl name qtype (Just initExpr) ->
            return $ ForInitDecl name qtype initExpr
          _ -> fail "Expected variable declaration with initializer in for loop",
      do
        Located exprSpan expr <- parseExpr
        return $ ForInitExpr (Located exprSpan expr)
    ]

parseStmtFor :: TokenParser (Located (Stmt ann))
parseStmtFor = do
  Located forSpan _ <- matchKeyword "for"
  Located lpanSpan _ <- matchSymbol "("
  maybeInit <- MP.optional parseForInit
  Located semi1Span _ <- matchSymbol ";"
  maybeCond <- MP.optional parseExpr
  Located semi2Span _ <- matchSymbol ";"
  maybePost <- MP.optional parseStmt
  Located rpanSpan _ <- matchSymbol ")"
  Located bodySpan bodyBlock <- parseBlock
  let combinedSpan = forSpan <> lpanSpan <> semi1Span <> semi2Span <> rpanSpan <> bodySpan
  return $ Located combinedSpan (StmtFor maybeInit maybeCond maybePost bodyBlock)

parseStmtIf :: TokenParser (Located (Stmt ann))
parseStmtIf = do
  Located ifSpan _ <- matchKeyword "if"
  Located lpanSpan _ <- matchSymbol "("
  Located condSpan condExpr <- parseExpr
  Located rpanSpan _ <- matchSymbol ")"
  Located thenSpan thenBlock <- parseBlock

  maybeElse <- MP.optional parseElseChain

  case maybeElse of
    Just (elseSpan, elseStmt) ->
      let combinedSpan = ifSpan <> lpanSpan <> condSpan <> rpanSpan <> thenSpan <> elseSpan
       in return $ Located combinedSpan (StmtIf (Located condSpan condExpr) thenBlock (Just elseStmt))
    Nothing ->
      let combinedSpan = ifSpan <> lpanSpan <> condSpan <> rpanSpan <> thenSpan
       in return $ Located combinedSpan (StmtIf (Located condSpan condExpr) thenBlock Nothing)

-- Parse the else chain (either "else if" or "else")
parseElseChain :: TokenParser (SourceSpan, Block ann)
parseElseChain = do
  Located elseSpan _ <- matchKeyword "else"
  maybeIf <- MP.optional (matchKeyword "if")
  case maybeIf of
    Just (Located ifSpan _) -> do
      Located lpanSpan _ <- matchSymbol "("
      Located condSpan condExpr <- parseExpr
      Located rpanSpan _ <- matchSymbol ")"
      Located thenSpan thenBlock <- parseBlock

      maybeNextElse <- MP.optional parseElseChain

      let (totalSpan, stmt) = case maybeNextElse of
            Just (nextElseSpan, nextElseStmt) ->
              let span = ifSpan <> lpanSpan <> condSpan <> rpanSpan <> thenSpan <> nextElseSpan
               in (elseSpan <> span, StmtIf (Located condSpan condExpr) thenBlock (Just nextElseStmt))
            Nothing ->
              let span = ifSpan <> lpanSpan <> condSpan <> rpanSpan <> thenSpan
               in (elseSpan <> span, StmtIf (Located condSpan condExpr) thenBlock Nothing)
      return (totalSpan, Block totalSpan [Located totalSpan stmt])

    Nothing -> do
      Located elseBlockSpan elseBlock <- parseBlock
      return (elseSpan <> elseBlockSpan, elseBlock)

parseStmt :: TokenParser (Located (Stmt ann))
parseStmt =
  MP.choice
    [ MP.try parseStmtVarDecl,
      MP.try parseStmtAssign,
      MP.try $ do
        expr <- parseLValue
        Located span _ <- matchSymbol "++"
        let combinedSpan = getSpan expr <> span
            litOne = Located span (ExprLiteral (LitInt (IntLiteral BaseDec 1)))
        return $ Located combinedSpan (StmtAssign expr AssignAdd litOne),
      MP.try $ do
        expr <- parseLValue
        Located span _ <- matchSymbol "--"
        let combinedSpan = getSpan expr <> span
            litOne = Located span (ExprLiteral (LitInt (IntLiteral BaseDec 1)))
        return $ Located combinedSpan (StmtAssign expr AssignSub litOne),
      do
        Located span _ <- matchSymbol "++"
        expr <- parseLValue
        let combinedSpan = span <> getSpan expr
            litOne = Located span (ExprLiteral (LitInt (IntLiteral BaseDec 1)))
        return $ Located combinedSpan (StmtAssign expr AssignAdd litOne),
      do
        Located span _ <- matchSymbol "--"
        expr <- parseLValue
        let combinedSpan = span <> getSpan expr
            litOne = Located span (ExprLiteral (LitInt (IntLiteral BaseDec 1)))
        return $ Located combinedSpan (StmtAssign expr AssignSub litOne),
      parseStmtExpr,
      parseStmtReturn,
      parseStmtBreak,
      parseStmtContinue,
      parseStmtBlock,
      parseStmtWhile,
      parseStmtFor,
      parseStmtIf
    ]
