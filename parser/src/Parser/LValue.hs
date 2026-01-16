module Parser.LValue where

import AST.Types.AST (LValue (LArrayIndex, LVarRef))
import AST.Types.Common (Located (..), VarName (..))
import Parser.Expr (parseExpr)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseLVarRef :: TokenParser (Located (LValue ann))
parseLVarRef = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (LVarRef (Located span (VarName name)))

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
