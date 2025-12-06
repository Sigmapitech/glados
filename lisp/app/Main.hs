module Main (main) where

import Lisp (entrypoint, options, prologue)
import Options.Applicative

main :: IO ()
main =
  execParser
    ( info
        (options <**> helper)
        ( fullDesc
            <> progDesc prologue
            <> header "Lisp"
        )
    )
    >>= entrypoint
