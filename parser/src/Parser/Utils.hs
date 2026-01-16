module Parser.Utils
  ( module Prelude,
    TokenParser,
    voidSpann,
    matchKeyword,
    matchSymbol,
    isIdentifier,
    isString,
    isBool,
    isSign,
    isInt,
    isIntDec,
  )
where

import AST.Types.Common (Located (..), SourceSpan, initialPos, spanSingle)
import AST.Types.Literal (IntBase (BaseDec))
import Data.Text (Text)
import Error (ParseError)
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

isIdentifier :: Located TokenContent -> Bool
isIdentifier (Located _ (TokIdentifier _)) = True
isIdentifier _ = False

isString :: Located TokenContent -> Bool
isString (Located _ (TokString _)) = True
isString _ = False

isBool :: Located TokenContent -> Bool
isBool (Located _ (TokBool _)) = True
isBool _ = False

isSign :: Located TokenContent -> Bool
isSign (Located _ (TokSymbol s)) = s == "+" || s == "-"
isSign _ = False

isInt :: Located TokenContent -> Bool
isInt (Located _ (TokInt _ _)) = True
isInt _ = False

isIntDec :: Located TokenContent -> Bool
isIntDec (Located _ (TokInt _ BaseDec)) = True
isIntDec _ = False
