module Main where

import AST.Types.AST (Program (..))
import Control.Monad (forM, when)
import qualified Data.Text.IO as TIO
import Formatter
  ( FormatOptions,
    defaultFormatOptions,
    formatOptionsCodec,
    formatProgram,
  )
import Lib (lexFile)
import Options.Applicative
  ( Alternative (some),
    Parser,
    ParserInfo,
    argument,
    execParser,
    flag,
    footer,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    progDesc,
    short,
    str,
    strOption,
    switch,
    (<**>),
  )
import Parser.Decl (parseDecl)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPrint, hPutStrLn, stderr)
import qualified Text.Megaparsec as MP
import qualified Toml

data Options = Options
  { optFiles :: [FilePath],
    optCheck :: Bool,
    optEmit :: EmitMode,
    optConfig :: Maybe FilePath,
    optVerbose :: Bool
  }

data EmitMode
  = EmitFiles -- Default: modify files in-place (like rustfmt)
  | EmitStdout -- Print to stdout
  deriving (Eq)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> some
      ( argument
          str
          ( metavar "FILES..."
              <> help "Quant source files to format"
          )
      )
    <*> switch
      ( long "check"
          <> help "Run in 'check' mode. Exits with 0 if input is formatted correctly. Exits with 1 if formatting is required."
      )
    <*> flag
      EmitFiles
      EmitStdout
      ( long "emit"
          <> help "Emit formatted output to stdout instead of modifying files"
      )
    <*> optional
      ( strOption
          ( long "config-path"
              <> metavar "PATH"
              <> help "Path to quantfmt.toml config file"
          )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Print verbose output"
      )

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Format Quant source files (like rustfmt)"
        <> header "quant fmt - Quant code formatter"
        <> footer "By default, quant fmt formats files in-place. Use --emit to print to stdout or --check to verify formatting."
    )

main :: IO ()
main = do
  options <- execParser opts

  -- Load config or use defaults
  formatOpts <- maybe findAndLoadConfig loadConfigOrDefault (optConfig options)

  -- Process all files
  results <- forM (optFiles options) $ processFile formatOpts options

  let allFormatted = and results
      numFiles = length results
      numNeedFormatting = length $ filter not results

  if optCheck options
    then do
      if allFormatted
        then do
          when (optVerbose options) $
            putStrLn $
              "✓ All " ++ show numFiles ++ " file(s) are formatted correctly"
          exitSuccess
        else do
          hPutStrLn stderr $
            "✗ " ++ show numNeedFormatting ++ " file(s) need formatting"
          exitFailure
    else do
      when (optVerbose options && optEmit options == EmitFiles) $
        putStrLn $
          "✓ Formatted " ++ show numFiles ++ " file(s)"
      if allFormatted
        then exitSuccess
        else exitFailure

processFile :: FormatOptions -> Options -> FilePath -> IO Bool
processFile formatOpts options filePath = do
  content <- TIO.readFile filePath

  -- Lex the file
  lexResult <- lexFile filePath
  case lexResult of
    Left err -> do
      hPutStrLn stderr $ "Error lexing " ++ filePath ++ ":"
      hPutStrLn stderr err
      return False
    Right tokens ->
      -- Parse declarations
      case MP.runParser (MP.many parseDecl) filePath tokens of
        Left err -> do
          hPutStrLn stderr $ "Error parsing " ++ filePath ++ ":"
          hPutStrLn stderr $ MP.errorBundlePretty err
          return False
        Right decls -> do
          let program = Program {unProgram = decls}
              formatted = formatProgram formatOpts program

          if optCheck options
            then do
              if content == formatted
                then do
                  when (optVerbose options) $
                    putStrLn $
                      "✓ " ++ filePath
                  return True
                else do
                  hPutStrLn stderr $ "✗ " ++ filePath
                  return False
            else case optEmit options of
              EmitStdout -> do
                TIO.putStrLn formatted
                return True
              EmitFiles -> do
                if content == formatted
                  then do
                    when (optVerbose options) $
                      putStrLn $
                        "  " ++ filePath ++ " (unchanged)"
                    return True
                  else do
                    TIO.writeFile filePath formatted
                    when (optVerbose options) $
                      putStrLn $
                        "✓ " ++ filePath
                    return True

-- | Load config from specific path or use defaults
loadConfigOrDefault :: FilePath -> IO FormatOptions
loadConfigOrDefault configPath = do
  exists <- doesFileExist configPath
  if exists
    then do
      result <- Toml.decodeFileEither formatOptionsCodec configPath
      case result of
        Left errs -> do
          hPutStrLn stderr $ "Warning: Failed to parse " ++ configPath ++ ":"
          hPrint stderr errs
          hPutStrLn stderr "Using default formatting options."
          return defaultFormatOptions
        Right formatOpts -> return formatOpts
    else do
      hPutStrLn stderr $ "Warning: Config file not found: " ++ configPath
      hPutStrLn stderr "Using default formatting options."
      return defaultFormatOptions

-- | Find and load config file from current directory
findAndLoadConfig :: IO FormatOptions
findAndLoadConfig = do
  cwd <- getCurrentDirectory
  let configPaths =
        [ cwd </> "quantfmt.toml",
          cwd </> ".quantfmt.toml"
        ]

  findConfig configPaths
  where
    findConfig [] = return defaultFormatOptions
    findConfig (path : paths) = do
      exists <- doesFileExist path
      if exists
        then do
          result <- Toml.decodeFileEither formatOptionsCodec path
          case result of
            Left errs -> do
              hPutStrLn stderr $ "Warning: Failed to parse " ++ path ++ ":"
              hPrint stderr errs
              findConfig paths
            Right formatOpts -> return formatOpts
        else findConfig paths
