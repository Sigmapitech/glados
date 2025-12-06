module Main (main) where

import Compiler (entrypoint, options, prologue)
import Options.Applicative

main :: IO ()
main =
  execParser
    ( info
        (options <**> helper)
        ( fullDesc
            <> progDesc prologue
            <> header "Compiler"
        )
    )
    >>= entrypoint
