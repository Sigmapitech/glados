module Main where

import Repl (ReplCommand (..), parseCommand)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: IO ()
spec = do
  describe "Repl - Command parsing" $ do
    it "parses :quit command" $ do
      parseCommand ":quit" `shouldBe` Quit
      parseCommand ":q" `shouldBe` Quit

    it "parses :help command" $ do
      parseCommand ":help" `shouldBe` Help
      parseCommand ":h" `shouldBe` Help
      parseCommand ":?" `shouldBe` Help

    it "parses :load command" $ do
      parseCommand ":load test.scm" `shouldBe` LoadFile "test.scm"
      parseCommand ":l file.lisp" `shouldBe` LoadFile "file.lisp"

    it "parses expressions as Eval" $ do
      parseCommand "(+ 1 2)" `shouldBe` Eval "(+ 1 2)"
      parseCommand "42" `shouldBe` Eval "42"
      parseCommand "(define x 10)" `shouldBe` Eval "(define x 10)"

    it "handles whitespace in load command" $ do
      parseCommand ":load  file.scm" `shouldBe` LoadFile " file.scm"

    it "treats unknown commands as expressions" $ do
      parseCommand ":unknown" `shouldBe` Eval ":unknown"
      parseCommand "not a command" `shouldBe` Eval "not a command"
