module Compiler (Options (..), options, entrypoint, prologue) where

import Options.Applicative

data Options = Options
  deriving (Show)

options :: Parser Options
options = pure Options

-- \^ Placeholder for actual optparse-applicative options

prologue :: String
prologue = "Compile a source file"

entrypoint :: Options -> IO ()
entrypoint _ = putStrLn "Compiling file"
