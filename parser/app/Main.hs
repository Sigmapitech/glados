module Main where

import Lib (lexFile)
import Parser.Decl (parseDecl)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Text.Megaparsec (many, runParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      result <- lexFile filePath
      case result of
        Left err -> do
          putStrLn err
          exitWith (ExitFailure 84)
        Right tokens -> do
          print tokens
          case runParser (many parseDecl) filePath tokens of
            Left parseErr -> do
              print parseErr
              exitWith (ExitFailure 84)
            Right ast -> do
              print ast
              exitSuccess
    _ -> do
      putStrLn "Usage: ./glados <file>"
      exitWith (ExitFailure 84)
