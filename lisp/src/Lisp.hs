module Lisp (Options (..), options, entrypoint, prologue) where

import Ast (unErrorMsg)
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
entrypoint opts = do
  input <- maybe getContents readFile (inputFile opts)
  sexprs <- case inputFile opts of
    Just file -> parseFile file
    Nothing -> parseString input
  case mapM sexprToAst sexprs of
    Left err -> do
      hPutStrLn stderr $ "AST conversion error: " ++ unErrorMsg err
      exitWith (ExitFailure 84)
    Right astList -> do
      let output = unlines $ map show astList
      case outputFile opts of
        Just file -> writeFile file output
        Nothing -> putStr output
