module Main where

import Lib (lexFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)

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
          -- For now, just print the token stream to prove the lexer works
          print tokens
          exitSuccess
    _ -> do
      putStrLn "Usage: ./glados <file>"
      exitWith (ExitFailure 84)
