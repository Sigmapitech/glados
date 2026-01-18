{-# LANGUAGE LambdaCase #-}

module Repl
  ( runRepl,
    ReplConfig (..),
    defaultReplConfig,
    ReplCommand (..),
    parseCommand,
  )
where

import qualified Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Lib (lexString)
import System.Console.Haskeline
  ( CompletionFunc,
    InputT,
    completeWord,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
    setComplete,
    simpleCompletion,
  )

data ReplConfig = ReplConfig
  { replPrompt :: String,
    replWelcome :: String,
    replGoodbye :: String,
    replShowEnv :: Bool
  }

defaultReplConfig :: ReplConfig
defaultReplConfig =
  ReplConfig
    { replPrompt = "parser> ",
      replWelcome =
        unlines
          [ "GLaDOS Parser REPL v0.1.0",
            "Type :help for available commands, :quit to exit"
          ],
      replGoodbye = "Goodbye!",
      replShowEnv = False
    }

data ReplCommand
  = Quit
  | Help
  | LoadFile FilePath
  | Eval String
  deriving (Show, Eq)

parseCommand :: String -> ReplCommand
parseCommand input
  | input == ":quit" || input == ":q" = Quit
  | input == ":help" || input == ":h" || input == ":?" = Help
  | ":load " `isPrefixOf` input = LoadFile (drop 6 input)
  | ":l " `isPrefixOf` input = LoadFile (drop 3 input)
  | otherwise = Eval input

runRepl :: ReplConfig -> IO ()
runRepl config = do
  putStrLn (replWelcome config)
  runInputT settings loop
  putStrLn (replGoodbye config)
  where
    settings = setComplete replCompletion defaultSettings

    loop :: InputT IO ()
    loop = do
      minput <- getInputLine (replPrompt config)
      case minput of
        Nothing -> return () -- EOF (Ctrl+D)
        Just input -> do
          let trimmed = dropWhile (== ' ') input
          if null trimmed
            then loop
            else do
              continue <- handleCommand (parseCommand trimmed)
              when continue loop

handleCommand :: ReplCommand -> InputT IO Bool
handleCommand = \case
  Quit -> return False
  Help -> do
    outputStrLn "Available commands:"
    outputStrLn "  :help, :h, :?     - Show this help message"
    outputStrLn "  :quit, :q         - Exit the REPL"
    outputStrLn "  :load <file>, :l  - Load and parse a file"
    outputStrLn ""
    outputStrLn "Enter any S-expression to parse it."
    return True
  LoadFile path -> do
    result <- liftIO $ loadAndParse path
    case result of
      Left err -> outputStrLn $ "Error loading file: " ++ err
      Right msg -> outputStrLn msg
    return True
  Eval input -> do
    case lexString input of
      Left err -> outputStrLn err
      Right tokens -> outputStrLn $ "Tokens: " ++ show tokens
    return True

-- | Load and parse a file
loadAndParse :: FilePath -> IO (Either String String)
loadAndParse path = do
  contentOrErr <- try (readFile path)
  case contentOrErr of
    Left err -> return $ Left $ show err
    Right content -> case lexString content of
      Left err -> return $ Left err
      Right tokens -> return $ Right $ "Loaded and parsed " ++ path ++ " successfully. Found " ++ show (length tokens) ++ " tokens."

-- | Try an IO action and catch IOError
try :: IO a -> IO (Either IOError a)
try action = Control.Exception.catch (Right <$> action) (return . Left)

replCompletion :: CompletionFunc IO
replCompletion = completeWord Nothing " \t" $ \word -> do
  let commands =
        [ ":help",
          ":quit",
          ":load",
          ":h",
          ":q",
          ":l"
        ]
      keywords =
        [ "define",
          "lambda",
          "if",
          "let"
        ]
      candidates = commands ++ keywords
      matches = filter (word `isPrefixOf`) candidates
  return $ map simpleCompletion matches
