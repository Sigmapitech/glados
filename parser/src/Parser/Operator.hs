module Parser.Operator
  ( parseAssignOp,
    parseBinaryOp,
    parseUnaryOp,
  )
where

import AST.Types.Common (Located (..))
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
import Parser.Utils
  ( TokenParser,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseOperator :: [Text] -> (Text -> Maybe a) -> TokenParser (Located a)
parseOperator validOps converter = do
  Located span (TokSymbol sym) <- MP.satisfy isValidSymbol
  case converter sym of
    Just op -> return $ Located span op
    Nothing -> MP.failure Nothing Set.empty
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
