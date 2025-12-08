module Lisp (Options (..), options, entrypoint, prologue) where

import AST (Environment, ErrorMsg, SExpr, initialEnv, isVoid, unErrorMsg)
import Control.Monad (unless)
import qualified Data.List.NonEmpty as NE
import Evaluator (evalManyFrom, evalManyToValue)
import Options.Applicative
import Parser (parseFile, parseString)
import SexprtoAST (sexprToAST)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

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

errorHelper :: [Char] -> ErrorMsg -> IO ()
errorHelper prefix errMsg =
  hPutStrLn stderr (prefix ++ unErrorMsg errMsg)
    >> exitWith (ExitFailure 84)

runFile :: FilePath -> Options -> IO ()
runFile file opts = parseFile file >>= evaluateSExpr opts

evaluateSExpr :: Options -> [SExpr] -> IO ()
evaluateSExpr opts = either onAstErr evalAst . mapM sexprToAST
  where
    onAstErr = errorHelper "AST error: "

    onEvalErr = errorHelper "*** Error : "
    onEvalOk vals =
      let output = unlines . map show . filter (not . isVoid) $ NE.toList vals
       in maybe putStr writeFile (outputFile opts) output

    evalAst asts =
      either onEvalErr onEvalOk (sequence $ evalManyToValue asts)

evaluateSExprWithEnv :: Options -> Environment -> [SExpr] -> IO ()
evaluateSExprWithEnv opts env = either onAstErr evalAst . mapM sexprToAST
  where
    onAstErr = errorHelper "AST error: "
    evalAst asts =
      let (results, env') = evalManyFrom env asts
       in either onEvalErr (`onEvalOk` env') (sequence results)

    onEvalErr = errorHelper "*** Error : "
    onEvalOk vals env' =
      let output = unlines . map show . filter (not . isVoid) $ NE.toList vals
       in maybe putStr writeFile (outputFile opts) output
            >> runReplWithEnv opts env'

runReplWithEnv :: Options -> Environment -> IO ()
runReplWithEnv opts env =
  putStr "> "
    >> getLine
    >>= \line ->
      unless
        (null line)
        (parseString line >>= evaluateSExprWithEnv opts env)
        >> runReplWithEnv opts env

entrypoint :: Options -> IO ()
entrypoint opts = case inputFile opts of
  Just file -> runFile file opts
  Nothing ->
    hSetBuffering stdout NoBuffering -- required to show the prompt
      >> runReplWithEnv opts initialEnv
