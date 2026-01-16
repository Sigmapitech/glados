module Tokens where

import AST.Types.Common (Located)
import AST.Types.Literal (IntBase (..))
import Data.Text (Text)
import Numeric (showHex, showOct)

data TokenContent
  = TokKeyword Text
  | TokIdentifier Text
  | TokSymbol Text
  | TokInt Integer IntBase
  | TokBool Bool
  | TokString Text
  | TokChar Char
  | TokEOF
  deriving (Eq, Ord)

showCon :: String -> [String] -> String
showCon name [] = name
showCon name args = name ++ " " ++ unwords args

instance Show TokenContent where
  show (TokKeyword kw) = showCon "TokKeyword" [show kw]
  show (TokIdentifier ident) = showCon "TokIdentifier" [show ident]
  show (TokSymbol sym) = showCon "TokSymbol" [show sym]
  show (TokInt val base) = showCon "TokInt" [showIntWithBase val base]
  show (TokBool b) = showCon "TokBool" [show b]
  show (TokString str) = showCon "TokString" [show str]
  show (TokChar c) = showCon "TokChar" [show c]
  show TokEOF = "TokEOF"

showIntWithBase :: Integer -> IntBase -> String
showIntWithBase val base = case base of
  BaseDec -> show val
  BaseHex
    | val < 0 -> "-0x" ++ showHex (negate val) ""
    | otherwise -> "0x" ++ showHex val ""
  BaseOct
    | val < 0 -> "-0o" ++ showOct (negate val) ""
    | otherwise -> "0o" ++ showOct val ""
  BaseBin
    | val < 0 -> "-0b" ++ showBin (negate val)
    | otherwise -> "0b" ++ showBin val

showBin :: Integer -> String
showBin n
  | n == 0 = "0"
  | otherwise = reverse $ showBin' n
  where
    showBin' 0 = ""
    showBin' x = (if odd x then '1' else '0') : showBin' (x `div` 2)

type Token = Located TokenContent
