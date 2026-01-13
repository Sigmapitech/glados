module Parser where

import AST.Types.Common (Located (..))
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
import qualified Data.Set as Set
import Data.Text (Text)
import Error (ParseError (..))
import qualified Text.Megaparsec as MP
import Tokens (Token, TokenConent (..))
import Prelude hiding (span)

type TokenParser = MP.Parsec ParseError [Token]

-- Helper to match a specific symbol
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
  Located span (TokInt n b) <- MP.satisfy isInt
  return $ Located span (IntLiteral b n)
  where
    isInt (Located _ (TokInt _ _)) = True
    isInt _ = False

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
