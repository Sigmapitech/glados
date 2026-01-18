module Main where

import Repl (defaultReplConfig, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl defaultReplConfig
    ["--help"] -> printHelp
    ["-h"] -> printHelp
    _ -> do
      putStrLn "Invalid arguments. Use --help for usage information."
      printHelp

printHelp :: IO ()
printHelp = do
  putStrLn "GLaDOS REPL - Interactive Interpreter"
  putStrLn ""
  putStrLn "Usage: glados-repl [OPTIONS]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help, -h    Show this help message"
  putStrLn ""
  putStrLn "Commands in REPL:"
  putStrLn "  :help, :h, :?     Show help"
  putStrLn "  :quit, :q         Exit"
  putStrLn "  :env, :e          Show environment"
  putStrLn "  :clear, :c        Clear environment"
  putStrLn "  :load <file>, :l  Load file"
