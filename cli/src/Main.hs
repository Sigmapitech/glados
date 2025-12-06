module Main where

import qualified Compiler (Options (..), entrypoint, options, prologue)
import qualified Lisp (Options (..), entrypoint, options, prologue)
import Options.Applicative

data Command
  = Compiler Compiler.Options
  | Lisp Lisp.Options
  deriving (Show)

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "compiler"
        ( info
            (Compiler <$> Compiler.options)
            ( progDesc Compiler.prologue
                <> fullDesc
            )
        )
        <> command
          "lisp"
          ( info
              (Lisp <$> Lisp.options)
              ( progDesc Lisp.prologue
                  <> fullDesc
              )
          )
          -- \| can add <> to add more subcommands here
    )

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "GLaDOS - Generic Language and Data Operand Syntax "
        <> header "glados cli"
    )

runCommand :: Command -> IO ()
runCommand (Compiler compOpts) = Compiler.entrypoint compOpts
runCommand (Lisp compOpts) = Lisp.entrypoint compOpts

main :: IO ()
main = execParser opts >>= runCommand
