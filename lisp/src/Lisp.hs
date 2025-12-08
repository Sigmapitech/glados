module Lisp (Options (..), options, entrypoint, prologue) where

import AST (isVoid, unErrorMsg)
import qualified Data.List.NonEmpty as NE
import Evaluator (evalManyToValue)
import Options.Applicative
import Parser (parseFile, parseString)
import SexprtoAST (sexprToAST)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { inputFile :: Maybe String,
    outputFile :: Maybe String
  }
  deriving (Show)

options :: Parser Options
options =
  Options
    <$> optional
      ( strArgument
          ( metavar "FILE"
              <> help "Input file to interpret"
          )
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Output file path (default=stdout)"
          )
      )

prologue :: String
prologue = "Interprets a LISP program"

entrypoint :: Options -> IO ()
entrypoint opts =
  maybe (getContents >>= parseString) parseFile (inputFile opts)
    >>= either onASTErr onASTOk . mapM sexprToAST
  where
    errorHelper prefix errMsg =
      hPutStrLn stderr (prefix ++ unErrorMsg errMsg)
        >> exitWith (ExitFailure 84)
    onASTErr = errorHelper "AST error: "
    onEvalErr = errorHelper "*** Error : "

    onASTOk asts =
      either
        onEvalErr
        (onEvalOk . NE.toList)
        (sequence (evalManyToValue asts))
    onEvalOk =
      maybe putStr writeFile (outputFile opts) . unlines . map show . filter (not . isVoid)
