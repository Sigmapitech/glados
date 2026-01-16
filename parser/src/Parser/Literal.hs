module Parser.Literal where

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
import Parser.Utils
  ( TokenParser,
    isBool,
    isInt,
    isSign,
    isString,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseStringLiteral :: TokenParser (Located StringLiteral)
parseStringLiteral = do
  Located span (TokString str) <- MP.satisfy isString
  return $ Located span (StringLiteral str)

parseBoolLiteral :: TokenParser (Located BoolLiteral)
parseBoolLiteral = do
  Located span (TokBool b) <- MP.satisfy isBool
  return $ Located span (BoolLiteral b)

parseIntLiteral :: TokenParser (Located IntLiteral)
parseIntLiteral = do
  maybeSign <- MP.optional (MP.satisfy isSign)
  Located span (TokInt n b) <- MP.satisfy isInt
  let signedN = case maybeSign of
        Just (Located _ (TokSymbol "-")) -> -n
        _ -> n
  return $ Located span (IntLiteral b signedN)

parseFloatLiteral :: TokenParser (Located FloatLiteral)
parseFloatLiteral = do
  Located intSpan (TokInt n BaseDec) <- MP.satisfy isInt
  Located decimalSpan _ <- matchSymbol "."
  Located fracSpan (TokInt fracN BaseDec) <- MP.satisfy isInt
  let floatVal = read (show n ++ "." ++ show fracN) :: Double
      combinedSpan = intSpan <> decimalSpan <> fracSpan
  return $ Located combinedSpan (FloatLiteral floatVal)

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
