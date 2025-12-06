module Lisp (Options (..), options, entrypoint, prologue) where

import Ast (unErrorMsg)
import Control.Monad ((>=>))
import Evaluator (evalAll)
import Options.Applicative
import Parser (parseFile, parseString)
import SexprtoAST (sexprToAst)
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
    >>= either onAstErr onAstOk . mapM sexprToAst
  where
    errorHelper prefix errMsg =
      hPutStrLn stderr (prefix ++ unErrorMsg errMsg)
        >> exitWith (ExitFailure 84)
    onAstErr = errorHelper "AST error: "
    onEvalErr = errorHelper "Evaluation error: "
    onAstOk = either onEvalErr onEvalOk . fst . evalAll
    onEvalOk =
      maybe putStr writeFile (outputFile opts) . unlines . map show
