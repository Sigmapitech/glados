module Parser where

import AST.Types.AST
import AST.Types.Common (FieldName (..), FuncName (..), Located (..), SourceSpan, TypeName (..), VarName (..), initialPos, spanSingle)
import AST.Types.Literal
  ( ArrayLiteral (..),
    BoolLiteral (..),
    FloatLiteral (..),
    IntBase (..),
    IntLiteral (..),
    Literal (..),
    StringLiteral (..),
  )
import AST.Types.Operator
  ( AssignOp,
    BinaryOp (..),
    UnaryOp,
    binaryOpPrecedence,
    symbolToAssignOp,
    symbolToBinaryOp,
    symbolToUnaryOp,
  )
import AST.Types.Type hiding (fieldName)
import qualified Data.Set as Set
import Data.Text (Text)
import Error (ParseError (..))
import qualified Text.Megaparsec as MP
import Tokens (Token, TokenContent (..))
import Prelude hiding (span)

type TokenParser = MP.Parsec ParseError [Token]

voidSpann :: SourceSpan
voidSpann = spanSingle (initialPos "/dev/null")

matchKeyword :: Text -> TokenParser (Located Text)
matchKeyword kw = do
  Located span (TokKeyword k) <- MP.satisfy isMatchingKeyword
  return $ Located span k
  where
    isMatchingKeyword (Located _ (TokKeyword k)) = k == kw
    isMatchingKeyword _ = False

matchSymbol :: Text -> TokenParser (Located Text)
matchSymbol sym = do
  Located span (TokSymbol s) <- MP.satisfy isMatchingSymbol
  return $ Located span s
  where
    isMatchingSymbol (Located _ (TokSymbol s)) = s == sym
    isMatchingSymbol _ = False

parseOperator :: [Text] -> (Text -> Maybe a) -> TokenParser (Located a)
parseOperator validOps converter = do
  Located span (TokSymbol sym) <- MP.satisfy isValidSymbol
  case converter sym of
    Just op -> return $ Located span op
    Nothing -> MP.failure Nothing Set.empty -- Should never happen due to isValidSymbol
  where
    isValidSymbol (Located _ (TokSymbol s)) = s `elem` validOps
    isValidSymbol _ = False

parseAssignOp :: TokenParser (Located AssignOp)
parseAssignOp = parseOperator assignOps symbolToAssignOp
  where
    assignOps = ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="]

parseBinaryOp :: TokenParser (Located BinaryOp)
parseBinaryOp = parseOperator binaryOps symbolToBinaryOp
  where
    binaryOps = ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||", "&", "|", "^", "<<", ">>"]

parseUnaryOp :: TokenParser (Located UnaryOp)
parseUnaryOp = parseOperator unaryOps symbolToUnaryOp
  where
    unaryOps = ["-", "!", "~", "+"]

parseStringLiteral :: TokenParser (Located StringLiteral)
parseStringLiteral = do
  Located span (TokString str) <- MP.satisfy isString
  return $ Located span (StringLiteral str)
  where
    isString (Located _ (TokString _)) = True
    isString _ = False

parseBoolLiteral :: TokenParser (Located BoolLiteral)
parseBoolLiteral = do
  Located span (TokBool b) <- MP.satisfy isBool
  return $ Located span (BoolLiteral b)
  where
    isBool (Located _ (TokBool _)) = True
    isBool _ = False

parseIntLiteral :: TokenParser (Located IntLiteral)
parseIntLiteral = do
  maybeSign <- MP.optional (MP.satisfy isSign)
  Located span (TokInt n b) <- MP.satisfy isInt
  let signedN = case maybeSign of
        Just (Located _ (TokSymbol "-")) -> -n
        _ -> n
  return $ Located span (IntLiteral b signedN)
  where
    isInt (Located _ (TokInt _ _)) = True
    isInt _ = False
    isSign (Located _ (TokSymbol s)) = s == "+" || s == "-"
    isSign _ = False

parseFloatLiteral :: TokenParser (Located FloatLiteral)
parseFloatLiteral = do
  Located intSpan (TokInt n BaseDec) <- MP.satisfy isInt
  Located decimalSpan _ <- matchSymbol "."
  Located fracSpan (TokInt fracN BaseDec) <- MP.satisfy isInt
  let floatVal = read (show n ++ "." ++ show fracN) :: Double
      combinedSpan = intSpan <> decimalSpan <> fracSpan
  return $ Located combinedSpan (FloatLiteral floatVal)
  where
    isInt (Located _ (TokInt _ _)) = True
    isInt _ = False

parseArrayLiteral :: TokenParser expr -> TokenParser (Located (ArrayLiteral expr))
parseArrayLiteral parseElement = do
  Located startSpan _ <- matchSymbol "["
  elements <- MP.sepBy parseElement (matchSymbol ",")
  Located endSpan _ <- matchSymbol "]"
  return $ Located (startSpan <> endSpan) (ArrayLiteral elements)

parseLiteral :: TokenParser expr -> TokenParser (Located (Literal expr))
parseLiteral parseExpre =
  MP.choice
    [ MP.try $ fmap LitFloat <$> parseFloatLiteral,
      fmap LitInt <$> parseIntLiteral,
      fmap LitBool <$> parseBoolLiteral,
      fmap LitString <$> parseStringLiteral,
      fmap LitArray <$> parseArrayLiteral parseExpre
    ]

parseSignedness :: TokenParser Signedness
parseSignedness = do
  Located _ (TokIdentifier s) <- MP.satisfy isSign
  case s of
    "s" -> return Signed
    "u" -> return Unsigned
    _ -> MP.failure Nothing Set.empty
  where
    isSign (Located _ (TokIdentifier s)) = s == "s" || s == "u"
    isSign _ = False

parseIntType :: TokenParser (Located IntType)
parseIntType = do
  Located span _ <- matchKeyword "int"
  maybeBracket <- MP.optional (matchSymbol "<")
  case maybeBracket of
    Nothing -> return $ Located span defaultIntType
    Just _ -> do
      Located _ (TokInt size BaseDec) <- MP.satisfy isInt
      maybeComma <- MP.optional (matchSymbol ",")
      sign <- case maybeComma of
        Nothing -> return Signed
        Just _ -> parseSignedness
      Located endSpan _ <- matchSymbol ">"
      let combinedSpan = span <> endSpan
      return $ Located combinedSpan (IntType (IntSize (fromInteger size)) sign)
  where
    isInt (Located _ (TokInt _ BaseDec)) = True
    isInt _ = False

parseFloatType :: TokenParser (Located FloatType)
parseFloatType = do
  Located span _ <- matchKeyword "float"
  maybeBracket <- MP.optional (matchSymbol "<")
  case maybeBracket of
    Nothing -> return $ Located span defaultFloatType
    Just _ -> do
      Located _ (TokInt size BaseDec) <- MP.satisfy isInt
      Located endSpan _ <- matchSymbol ">"
      let floatSize = case size of
            32 -> Float32
            64 -> Float64
            _ -> Float64
          combinedSpan = span <> endSpan
      return $ Located combinedSpan (FloatType floatSize)
  where
    isInt (Located _ (TokInt _ BaseDec)) = True
    isInt _ = False

parsePrimitiveType :: TokenParser (Located PrimitiveType)
parsePrimitiveType =
  MP.choice
    [ fmap PrimInt <$> parseIntType,
      fmap PrimFloat <$> parseFloatType,
      do
        Located span _ <- matchKeyword "bool"
        return $ Located span PrimBool,
      do
        Located span _ <- matchKeyword "str"
        return $ Located span PrimString,
      do
        Located span _ <- matchKeyword "void"
        return $ Located span PrimNone
    ]

parseArrayType :: TokenParser (Located ArrayType)
parseArrayType = do
  Located span _ <- matchSymbol "["
  Located elemSpan elemType <- parseType
  Located endSpan _ <- matchSymbol "]"
  let combinedSpan = span <> elemSpan <> endSpan
  return $ Located combinedSpan (ArrayType elemType)

parseConstness :: TokenParser (Located Constness)
parseConstness = do
  maybeConst <- MP.optional (matchKeyword "const")
  case maybeConst of
    Just (Located span _) -> return $ Located span Const
    Nothing -> return $ Located voidSpann Mutable

parseQualifiedType :: TokenParser (Located QualifiedType)
parseQualifiedType = do
  Located constSpan constness <- parseConstness
  Located typeSpan typ <- parseType
  let combinedSpan = constSpan <> typeSpan
  return $ Located combinedSpan (QualifiedType constness typ)

parseParameter :: TokenParser (Located Parameter)
parseParameter = do
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  Located colonSpan _ <- matchSymbol ":"
  Located typeSpan qtype <- parseQualifiedType
  let combinedSpan = nameSpan <> colonSpan <> typeSpan
  return $ Located combinedSpan (Parameter (VarName name) qtype)
  where
    isIdentifier (Located _ (TokIdentifier _)) = True
    isIdentifier _ = False

parseFunctionType :: TokenParser (Located FunctionType)
parseFunctionType = do
  Located span _ <- matchSymbol "("
  params <- MP.sepBy parseParameter (matchSymbol ",")
  Located closeSpan _ <- matchSymbol ")"
  Located arrowSpan _ <- matchSymbol "->"
  Located returnTypeSpan retType <- parseType
  let combinedSpan = span <> closeSpan <> arrowSpan <> returnTypeSpan
  return $ Located combinedSpan (FunctionType (map (\(Located _ p) -> p) params) retType)

parseTypeNamed :: TokenParser (Located TypeName)
parseTypeNamed = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (TypeName name)
  where
    isIdentifier (Located _ (TokIdentifier _)) = True
    isIdentifier _ = False

parseType :: TokenParser (Located Type)
parseType =
  MP.choice
    [ fmap TypePrimitive <$> parsePrimitiveType,
      fmap TypeArray <$> parseArrayType,
      fmap TypeFunction <$> parseFunctionType
    ]

parseExprVar :: TokenParser (Located (Expr ann))
parseExprVar = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (ExprVar (Located span (VarName name)))
  where
    isIdentifier (Located _ (TokIdentifier _)) = True
    isIdentifier _ = False

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

parseFieldInit :: TokenParser (Located FieldName, Located (Expr ann))
parseFieldInit = do
  Located fieldSpan (TokIdentifier fieldName) <- MP.satisfy isIdentifier
  _ <- matchSymbol "="
  expr <- parseExpr
  return (Located fieldSpan (FieldName fieldName), expr)
  where
    isIdentifier (Located _ (TokIdentifier _)) = True
    isIdentifier _ = False

parseExprArrayInit :: TokenParser (Located (Expr ann))
parseExprArrayInit = do
  locatedType <- parseType
  _ <- matchSymbol "{"
  exprs <- MP.sepBy parseExpr (matchSymbol ",")
  Located endSpan _ <- matchSymbol "}"
  let Located typeSpan _ = locatedType
  return $ Located (typeSpan <> endSpan) (ExprArrayInit locatedType exprs)

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

    isIdentifier (Located _ (TokIdentifier _)) = True
    isIdentifier _ = False

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
  where
    isIdentifier (Located _ (TokIdentifier _)) = True
    isIdentifier _ = False

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

parseBlock :: TokenParser (Located (Block ann))
parseBlock = do
  Located startSpan _ <- matchSymbol "{"
  stmts <- MP.sepBy parseStmt (matchSymbol ";")
  Located endSpan _ <- matchSymbol "}"
  let combinedSpan = startSpan <> endSpan
  return $ Located combinedSpan (Block combinedSpan stmts)

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

parseLVarRef :: TokenParser (Located (LValue ann))
parseLVarRef = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (LVarRef (Located span (VarName name)))
  where
    isIdentifier (Located _ (TokIdentifier _)) = True
    isIdentifier _ = False

parseLArrayIndex :: Located (LValue ann) -> TokenParser (Located (LValue ann))
parseLArrayIndex base = do
  _ <- matchSymbol "["
  index <- parseExpr
  Located endSpan _ <- matchSymbol "]"
  let Located baseSpan _ = base
  return $ Located (baseSpan <> endSpan) (LArrayIndex base index)

parseLValue :: TokenParser (Located (LValue ann))
parseLValue =
  MP.choice
    [ do
        base <- parseLVarRef
        parseLValueSuffixes base,
      parseLVarRef
    ]
  where
    parseLValueSuffixes lvalue = do
      maybeSuffix <- MP.optional $ parseLArrayIndex lvalue
      case maybeSuffix of
        Just newLValue -> parseLValueSuffixes newLValue
        Nothing -> return lvalue

parseVisibility :: TokenParser (Located Visibility)
parseVisibility = do
  maybeStatic <- MP.optional (matchKeyword "static")
  case maybeStatic of
    Just (Located span _) -> return $ Located span Static
    Nothing -> return $ Located voidSpann Public

parseDeclFunction :: TokenParser (Located (Decl ann))
parseDeclFunction = do
  Located visSpan visibility <- parseVisibility
  Located fnSpan _ <- matchKeyword "fn"
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  Located typeSpan funcType <- parseFunctionType
  Located bodySpan block <- parseBlock

  let combinedSpan = case visibility of
        Static -> visSpan <> fnSpan <> nameSpan <> typeSpan <> bodySpan
        Public -> fnSpan <> nameSpan <> typeSpan <> bodySpan

  let functionDecl =
        FunctionDecl
          { funcDeclName = Located nameSpan (FuncName name),
            funcDeclParams = map (Located voidSpann) (funcParams funcType),
            funcDeclReturnType = Located voidSpann (funcReturnType funcType),
            funcDeclBody = block
          }
  return $ Located combinedSpan (DeclFunction visibility functionDecl)
  where
    isIdentifier (Located _ (TokIdentifier _)) = True
    isIdentifier _ = False
