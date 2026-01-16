module Parser.Type where

import AST.Types.Common (Located (..), TypeName (..), VarName (..), getSpan)
import AST.Types.Literal (IntBase (BaseDec))
import AST.Types.Type
  ( ArrayType (ArrayType),
    Constness (..),
    FloatSize (Float32, Float64),
    FloatType (FloatType),
    FunctionType (FunctionType),
    IntSize (IntSize),
    IntType (IntType),
    Parameter (Parameter),
    PrimitiveType (..),
    QualifiedType (QualifiedType),
    Signedness (..),
    Type (TypeArray, TypeFunction, TypePrimitive),
    defaultFloatType,
    defaultIntType,
  )
import qualified Data.Set as Set
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    isIntDec,
    matchKeyword,
    matchSymbol,
    voidSpann,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseSignedness :: TokenParser Signedness
parseSignedness = do
  Located _ (TokIdentifier s) <- MP.satisfy isSignednessId
  case s of
    "s" -> return Signed
    "u" -> return Unsigned
    _ -> MP.failure Nothing Set.empty
  where
    isSignednessId (Located _ (TokIdentifier s)) = s == "s" || s == "u"
    isSignednessId _ = False

parseIntType :: TokenParser (Located IntType)
parseIntType = do
  Located span _ <- matchKeyword "int"
  maybeBracket <- MP.optional (matchSymbol "<")
  case maybeBracket of
    Nothing -> return $ Located span defaultIntType
    Just _ -> do
      Located _ (TokInt size BaseDec) <- MP.satisfy isIntDec
      maybeComma <- MP.optional (matchSymbol ",")
      sign <- case maybeComma of
        Nothing -> return Signed
        Just _ -> parseSignedness
      Located endSpan _ <- matchSymbol ">"
      let combinedSpan = span <> endSpan
      return $ Located combinedSpan (IntType (IntSize (fromInteger size)) sign)

parseFloatType :: TokenParser (Located FloatType)
parseFloatType = do
  Located span _ <- matchKeyword "float"
  maybeBracket <- MP.optional (matchSymbol "<")
  case maybeBracket of
    Nothing -> return $ Located span defaultFloatType
    Just _ -> do
      Located _ (TokInt size BaseDec) <- MP.satisfy isIntDec
      Located endSpan _ <- matchSymbol ">"
      let floatSize = case size of
            32 -> Float32
            64 -> Float64
            _ -> Float64
          combinedSpan = span <> endSpan
      return $ Located combinedSpan (FloatType floatSize)

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
  Located elemSpan elemType <- parseQualifiedType
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

parseFunctionType :: TokenParser (Located FunctionType)
parseFunctionType = do
  Located span _ <- matchSymbol "("
  params <- MP.sepBy parseParameter (matchSymbol ",")
  Located closeSpan _ <- matchSymbol ")"
  Located arrowSpan _ <- matchSymbol "->"
  ret <- parseQualifiedType
  let combinedSpan = span <> closeSpan <> arrowSpan <> getSpan ret
  return $ Located combinedSpan (FunctionType params ret)

parseTypeNamed :: TokenParser (Located TypeName)
parseTypeNamed = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (TypeName name)

parseType :: TokenParser (Located Type)
parseType =
  MP.choice
    [ fmap TypePrimitive <$> parsePrimitiveType,
      fmap TypeArray <$> parseArrayType,
      fmap TypeFunction <$> parseFunctionType
    ]
