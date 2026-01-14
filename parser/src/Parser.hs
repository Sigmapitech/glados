module Parser where

import AST.Types.Common (Located (..), TypeName (..), VarName (..), initialPos, spanSingle)
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
    BinaryOp,
    UnaryOp,
    symbolToAssignOp,
    symbolToBinaryOp,
    symbolToUnaryOp,
  )
import AST.Types.Type
import qualified Data.Set as Set
import Data.Text (Text)
import Error (ParseError (..))
import qualified Text.Megaparsec as MP
import Tokens (Token, TokenContent (..))
import Prelude hiding (span)

type TokenParser = MP.Parsec ParseError [Token]

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
parseLiteral parseExpr =
  MP.choice
    [ fmap LitFloat <$> parseFloatLiteral,
      fmap LitInt <$> parseIntLiteral,
      fmap LitBool <$> parseBoolLiteral,
      fmap LitString <$> parseStringLiteral,
      fmap LitArray <$> parseArrayLiteral parseExpr
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
    Nothing -> return $ Located (spanSingle (initialPos "<stream>")) Mutable

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
