{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Tokens where

import AST.Types.Common
  ( Column (..),
    FilePath' (..),
    Line (..),
    Located (..),
    SourceSpan (..),
  )
import qualified AST.Types.Common as AST
import AST.Types.Literal (IntBase (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex, showOct)
import Text.Megaparsec (PosState (..), SourcePos (..))
import Text.Megaparsec.Pos (mkPos)
import Text.Megaparsec.Stream (TraversableStream (..), VisualStream (..), showTokens)
import Prelude hiding (span)

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

-- ANSI color codes for error messages
tokenRed, tokenGreen, tokenYellow, tokenBlue, tokenMagenta, tokenCyan :: String
tokenReset, tokenBold :: String
tokenRed = "\ESC[31m"
tokenGreen = "\ESC[32m"
tokenYellow = "\ESC[33m"
tokenBlue = "\ESC[34m"
tokenMagenta = "\ESC[35m"
tokenCyan = "\ESC[36m"

tokenReset = "\ESC[0m"

tokenBold = "\ESC[1m"

instance VisualStream [Token] where
  showTokens :: Proxy [Token] -> NonEmpty Token -> String
  showTokens _ tokens =
    let tokenStrs = NE.toList $ fmap formatToken tokens
     in tokenBold ++ tokenMagenta ++ unwords tokenStrs ++ tokenReset
    where
      formatToken :: Token -> String
      formatToken (Located _ content) = case content of
        TokKeyword kw -> tokenCyan ++ "keyword " ++ show kw ++ tokenMagenta
        TokIdentifier ident -> tokenGreen ++ "identifier " ++ show ident ++ tokenMagenta
        TokSymbol sym -> tokenYellow ++ "symbol " ++ show sym ++ tokenMagenta
        TokInt val _ -> tokenBlue ++ "number " ++ show val ++ tokenMagenta
        TokBool b -> tokenBlue ++ "boolean " ++ show b ++ tokenMagenta
        TokString str -> tokenGreen ++ "string " ++ show str ++ tokenMagenta
        TokChar c -> tokenGreen ++ "char " ++ show c ++ tokenMagenta
        TokEOF -> tokenRed ++ "end-of-file" ++ tokenMagenta

instance TraversableStream [Token] where
  reachOffset :: Int -> PosState [Token] -> (Maybe String, PosState [Token])
  reachOffset o pst@PosState {..} =
    case drop (o - pstateOffset) pstateInput of
      [] -> (Nothing, pst {pstateOffset = o})
      (Located span _ : _) ->
        let sourcePos = case span of
              SourceSpan start _ ->
                let AST.SourcePos (FilePath' path) (Line line) (Column col) _ = start
                 in SourcePos (T.unpack path) (mkPos $ fromIntegral line) (mkPos $ fromIntegral col)
         in ( Nothing,
              pst
                { pstateInput = drop (o - pstateOffset) pstateInput,
                  pstateOffset = o,
                  pstateSourcePos = sourcePos
                }
            )
