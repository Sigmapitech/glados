{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpecIntegration (specIntegration) where

import AST.Lisp.AST (RuntimeValue (..), unErrorMsg)
import AST.Lisp.SexprtoAST (sexprToAST)
import Control.Exception (SomeException, catch)
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NE
import Evaluator (evalManyToValue)
import Parser (parseFile)
import System.Directory (doesFileExist)
import Test.Hspec

evalFile :: FilePath -> IO (Either String [RuntimeValue])
evalFile path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ "File not found: " ++ path
    else
      catch
        ( do
            sexprs <- parseFile path
            return $ case mapM sexprToAST sexprs of
              Left err -> Left $ "AST conversion error: " ++ unErrorMsg err
              Right asts -> case sequence (evalManyToValue asts) of
                Left err -> Left $ "Evaluation error: " ++ unErrorMsg err
                Right values -> Right $ NE.toList values
        )
        (\(e :: SomeException) -> return $ Left $ "Parse error: " ++ show e)

shouldEvalTo :: FilePath -> [RuntimeValue] -> Expectation
shouldEvalTo path expected = do
  result <- evalFile path
  case result of
    Left err -> expectationFailure $ "Evaluation failed: " ++ err
    Right values -> values `shouldBe` expected

shouldFailWith :: FilePath -> String -> Expectation
shouldFailWith path expectedErr = do
  result <- evalFile path
  case result of
    Left err -> err `shouldContain` expectedErr
    Right values -> expectationFailure $ "Expected failure but got: " ++ show values

specIntegration :: Spec
specIntegration = do
  describe "Integration Tests - Full Pipeline (Parse -> AST -> Eval)" $ do
    describe "Assignment Sample Files" $ do
      it "evaluates factorial.scm correctly" $ do
        result <- evalFile "tests/fixtures/assignment-samples/factorial.scm"
        result `shouldSatisfy` \case
          Right vals -> last vals == VInt 3628800
          Left _ -> False

      it "evaluates builtins1.scm - arithmetic operations" $ do
        "tests/fixtures/assignment-samples/builtins1.scm"
          `shouldEvalTo` [VInt 11]

      it "evaluates builtins2.scm - comparisons" $ do
        "tests/fixtures/assignment-samples/builtins2.scm"
          `shouldEvalTo` [VBool True]

      it "evaluates builtins3.scm - nested arithmetic" $ do
        "tests/fixtures/assignment-samples/builtins3.scm"
          `shouldEvalTo` [VBool False]

      it "evaluates call.scm - function calls" $ do
        "tests/fixtures/assignment-samples/call.scm"
          `shouldEvalTo` [VInt 5]

      it "evaluates foo.scm - variable definition and usage" $ do
        "tests/fixtures/assignment-samples/foo.scm"
          `shouldEvalTo` [VUnit, VInt 42]

      it "evaluates function1.scm - function definition" $ do
        result <- evalFile "tests/fixtures/assignment-samples/function1.scm"
        result `shouldSatisfy` \case
          Right vals -> last vals == VInt 7
          Left _ -> False

      it "evaluates if1.scm - simple conditional" $ do
        "tests/fixtures/assignment-samples/if1.scm"
          `shouldEvalTo` [VInt 1]

      it "evaluates if2.scm - nested conditionals" $ do
        "tests/fixtures/assignment-samples/if2.scm"
          `shouldEvalTo` [VInt 2]

      it "evaluates if3.scm - conditional with function calls" $ do
        "tests/fixtures/assignment-samples/if3.scm"
          `shouldEvalTo` [VUnit, VInt 21]

      it "evaluates lambda1.scm - simple lambda (returns procedure)" $ do
        result <- evalFile "tests/fixtures/assignment-samples/lambda1.scm"
        result `shouldSatisfy` \case
          Right [VProcedure {}] -> True
          _ -> False

      it "evaluates lambda2.scm - lambda with immediate call" $ do
        "tests/fixtures/assignment-samples/lambda2.scm"
          `shouldEvalTo` [VInt 3]

      it "evaluates lambda3.scm - lambda assigned to variable" $ do
        "tests/fixtures/assignment-samples/lambda3.scm"
          `shouldEvalTo` [VUnit, VInt 7]

      it "evaluates superior.scm - custom comparison function" $ do
        "tests/fixtures/assignment-samples/superior.scm"
          `shouldEvalTo` [VUnit, VBool True]

      it "handles error.scm - error cases" $ do
        "tests/fixtures/assignment-samples/error.scm"
          `shouldFailWith` "variable foo is not bound"

    describe "Complex Examples" $ do
      it "evaluates complex.lisp with multiple functions" $ do
        result <- evalFile "tests/fixtures/complex.lisp"
        result `shouldSatisfy` \case
          Right values -> not (null values)
          Left _ -> False

    describe "Error Handling" $ do
      it "handles non-existent files" $ do
        result <- evalFile "tests/fixtures/non-existent.scm"
        result `shouldSatisfy` \case
          Left err -> "File not found" `isInfixOf` err
          Right _ -> False

      it "handles invalid syntax in testInvalidFile.ss" $ do
        result <- evalFile "tests/fixtures/testInvalidFile.ss"
        result `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False

      it "evaluates multiple top-level expressions" $ do
        result <- evalFile "tests/fixtures/assignment-samples/foo.scm"
        result `shouldSatisfy` \case
          Right values -> length values == 2 && last values == VInt 42
          Left _ -> False

      it "maintains state across definitions" $ do
        result <- evalFile "tests/fixtures/assignment-samples/function1.scm"
        result `shouldSatisfy` \case
          Right vals -> last vals == VInt 7
          Left _ -> False

    describe "Performance Tests" $ do
      it "handles deeply recursive functions (factorial)" $ do
        result <- evalFile "tests/fixtures/assignment-samples/factorial.scm"
        result `shouldSatisfy` \case
          Right vals -> last vals == VInt 3628800
          _ -> False

      it "handles complex nested expressions" $ do
        result <- evalFile "tests/fixtures/assignment-samples/builtins3.scm"
        result `shouldSatisfy` \case
          Right vals -> last vals == VBool False
          _ -> False
