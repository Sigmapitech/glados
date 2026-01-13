module Parser where

import AST.Types.AST
import AST.Types.Common (Located (..), SourceSpan (..))
import AST.Types.Operator
import qualified Data.Set as Set
import Error (ParseError (..))
import Text.Megaparsec (ParseErrorBundle)
import qualified Text.Megaparsec as MP
import Tokens (Token, TokenConent (..))

type TokenParser = MP.Parsec ParseError [Token]

parseAssignOp :: TokenParser (Located AssignOp)
parseAssignOp = do
  Located span (TokSymbol sym) <- MP.satisfy isAssignSymbol
  case symbolToAssignOp sym of
    Just op -> return $ Located span op
    Nothing -> MP.failure Nothing Set.empty -- Should never happen due to isAssignSymbol
  where
    assignOps = ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="]

    isAssignSymbol (Located _ (TokSymbol s)) = s `elem` assignOps
    isAssignSymbol _ = False
