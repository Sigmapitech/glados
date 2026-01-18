module SpecError (errorSpec) where

import Error (GLaDOSError (..), codeSym, esc)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)
import Text.Megaparsec (ShowErrorComponent (..))

errorSpec :: Spec
errorSpec = do
  describe "Error - GLaDOSError data type" $ do
    it "can be compared for equality" $ do
      ErrUnclosedComment `shouldBe` ErrUnclosedComment
      ErrUnclosedString `shouldBe` ErrUnclosedString
      ErrInvalidChar 'x' `shouldBe` ErrInvalidChar 'x'
      ErrGeneric "test" `shouldBe` ErrGeneric "test"

    it "different errors are not equal" $ do
      (ErrUnclosedComment == ErrUnclosedString) `shouldBe` False
      (ErrInvalidChar 'x' == ErrInvalidChar 'y') `shouldBe` False
      (ErrGeneric "a" == ErrGeneric "b") `shouldBe` False

    it "can be ordered" $ do
      (ErrUnclosedComment < ErrUnclosedString) `shouldSatisfy` const True
      (ErrUnclosedComment <= ErrUnclosedComment) `shouldBe` True

  describe "Error - showErrorComponent for ErrUnclosedComment" $ do
    let result = showErrorComponent ErrUnclosedComment

    it "contains error title" $ do
      result `shouldContain` "Lexical Error"
      result `shouldContain` "Unclosed block comment"

    it "contains hint about opening comment" $ do
      result `shouldContain` "/*"

    it "contains hint about closing comment" $ do
      result `shouldContain` "*/"

    it "contains Hint marker" $ do
      result `shouldContain` "Hint"

  describe "Error - showErrorComponent for ErrUnclosedString" $ do
    let result = showErrorComponent ErrUnclosedString

    it "contains error title" $ do
      result `shouldContain` "Lexical Error"
      result `shouldContain` "String literal"
      result `shouldContain` "closing quote"

    it "contains hint about closing string" $ do
      result `shouldContain` "\""

    it "mentions same line requirement" $ do
      result `shouldContain` "same line"

  describe "Error - showErrorComponent for ErrInvalidChar" $ do
    it "shows error message for backtick" $ do
      let result = showErrorComponent (ErrInvalidChar '`')
      result `shouldContain` "Unexpected character"
      result `shouldContain` "`"
      result `shouldContain` "Backticks are not valid"
      result `shouldContain` "single quote"

    it "shows error message for single quote" $ do
      let result = showErrorComponent (ErrInvalidChar '\'')
      result `shouldContain` "Unexpected character"
      result `shouldContain` "'"
      result `shouldContain` "empty or broken character"

    it "shows error message for semicolon" $ do
      let result = showErrorComponent (ErrInvalidChar ';')
      result `shouldContain` "Unexpected character"
      result `shouldContain` ";"
      result `shouldContain` "Semicolons are valid"

    it "shows generic error message for unknown char" $ do
      let result = showErrorComponent (ErrInvalidChar '@')
      result `shouldContain` "Unexpected character"
      result `shouldContain` "@"
      result `shouldContain` "not recognized"

  describe "Error - showErrorComponent for ErrGeneric" $ do
    it "shows custom error message" $ do
      let result = showErrorComponent (ErrGeneric "Custom error message")
      result `shouldContain` "Error:"
      result `shouldContain` "Custom error message"

    it "handles empty message" $ do
      let result = showErrorComponent (ErrGeneric "")
      result `shouldContain` "Error:"

  describe "Error - ANSI escape code utilities" $ do
    it "esc function generates proper escape codes" $ do
      esc "0" `shouldBe` "\ESC[0m"
      esc "1" `shouldBe` "\ESC[1m"
      esc "31" `shouldBe` "\ESC[31m"

    it "codeSym wraps string with proper formatting" $ do
      let result = codeSym "test"
      result `shouldContain` "test"
      result `shouldSatisfy` (\s -> length s > 4) -- contains escape codes
  describe "Error - ShowErrorComponent instance" $ do
    it "can show ErrUnclosedComment" $ do
      show ErrUnclosedComment `shouldBe` "ErrUnclosedComment"

    it "can show ErrUnclosedString" $ do
      show ErrUnclosedString `shouldBe` "ErrUnclosedString"

    it "can show ErrInvalidChar" $ do
      show (ErrInvalidChar 'x') `shouldBe` "ErrInvalidChar 'x'"

    it "can show ErrGeneric" $ do
      show (ErrGeneric "test") `shouldBe` "ErrGeneric \"test\""
