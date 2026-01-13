module Parser where

import AST.Types.AST
import AST.Types.Common (Located (..), SourceSpan (..))
import AST.Types.Literal
import AST.Types.Operator
import qualified Data.Set as Set
import Data.Text (Text)
import Error (ParseError (..))
import Text.Megaparsec (ParseErrorBundle)
import qualified Text.Megaparsec as MP
import Tokens (Token, TokenConent (..))
import Prelude hiding (span)

type TokenParser = MP.Parsec ParseError [Token]

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
  Located span (TokFloat f) <- MP.satisfy isFloat
  return $ Located span (FloatLiteral f)
  where
    isFloat (Located _ (TokFloat _)) = True
    isFloat _ = False
