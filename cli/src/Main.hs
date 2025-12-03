module Main where

import qualified Compiler (Options (..), entrypoint, options, prologue)
import Options.Applicative

newtype Command
  = Compiler Compiler.Options
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
    )

-- \| can <> to add more subcommands here

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "GLaDOS - A multi-purpose language tool"
        <> header "glados cli"
    )

runCommand :: Command -> IO ()
runCommand (Compiler compOpts) = Compiler.entrypoint compOpts

main :: IO ()
main = execParser opts >>= runCommand
