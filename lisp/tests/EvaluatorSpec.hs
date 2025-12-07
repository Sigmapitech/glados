{-# LANGUAGE OverloadedStrings #-}

module EvaluatorSpec (spec) where

import Ast
import Data.Either (isLeft)
import qualified Data.List.NonEmpty as NE
import Evaluator (eval, evalFrom, evalMany, evalManyToValue, evalToValue)
import Test.Hspec

-- | Test suite for literal evaluation
literalEvalSpec :: Spec
literalEvalSpec = do
  describe "Literals" $ do
    it "evaluates integer literals" $ do
      evalToValue (LiteralInt 42) `shouldBe` Right (VInt 42)
      evalToValue (LiteralInt 0) `shouldBe` Right (VInt 0)
      evalToValue (LiteralInt (-100)) `shouldBe` Right (VInt (-100))

    it "evaluates boolean literals" $ do
      evalToValue (LiteralBool True) `shouldBe` Right (VBool True)
      evalToValue (LiteralBool False) `shouldBe` Right (VBool False)

-- | Test suite for variable operations
variableEvalSpec :: Spec
variableEvalSpec = do
  describe "Variables" $ do
    it "looks up variables in environment" $ do
      let env = extendEnv "x" (VInt 10) emptyEnv
          result = NE.head $ fst $ evalFrom env (VariableRef "x")
      result `shouldBe` Right (VInt 10)

    it "looks up multiple variables" $ do
      let env = extendEnv "y" (VBool True) $ extendEnv "x" (VInt 10) emptyEnv
      NE.head (fst $ evalFrom env (VariableRef "x")) `shouldBe` Right (VInt 10)
      NE.head (fst $ evalFrom env (VariableRef "y")) `shouldBe` Right (VBool True)

    it "errors on undefined variables" $ do
      evalToValue (VariableRef "undefined") `shouldSatisfy` isLeft

    it "provides meaningful error message for undefined variables" $ do
      case evalToValue (VariableRef "nonexistent") of
        Left (ErrorMsg msg) -> msg `shouldContain` "variable nonexistent is not bound."
        Right _ -> expectationFailure "Expected error for undefined variable"

-- | Test suite for define special form
defineEvalSpec :: Spec
defineEvalSpec = do
  describe "Define" $ do
    describe "Variable Definitions" $ do
      it "defines a variable with literal value" $ do
        let ast = Define "x" (LiteralInt 5)
            (results, env) = eval ast
            result = NE.head results
        result `shouldBe` Right VUnit
        lookupEnv "x" env `shouldBe` Just (VInt 5)

      it "defines a variable with boolean value" $ do
        let ast = Define "flag" (LiteralBool True)
            (_, env) = eval ast
        lookupEnv "flag" env `shouldBe` Just (VBool True)

      it "defines with computed expression" $ do
        let ast = Define "y" (Call (VariableRef "+") [LiteralInt 2, LiteralInt 3])
            (_, env) = eval ast
        lookupEnv "y" env `shouldBe` Just (VInt 5)

      it "allows shadowing previous definitions" $ do
        let asts = [Define "x" (LiteralInt 10), Define "x" (LiteralInt 20)]
            (results, env) = evalMany asts
        results `shouldBe` NE.fromList [Right VUnit, Right VUnit]
        lookupEnv "x" env `shouldBe` Just (VInt 20)

    describe "Function Definitions" $ do
      it "defines a function with lambda" $ do
        let lambda = Lambda ["x"] (Call (VariableRef "+") [VariableRef "x", LiteralInt 1])
            ast = Define "increment" lambda
            (results, env) = eval ast
        NE.head results `shouldBe` Right VUnit
        lookupEnv "increment" env `shouldBe` Just (VProcedure ["x"] (Call (VariableRef "+") [VariableRef "x", LiteralInt 1]))

      it "defines a function and calls it" $ do
        let lambda = Lambda ["x"] (Call (VariableRef "*") [VariableRef "x", LiteralInt 2])
            asts = [Define "double" lambda, Call (VariableRef "double") [LiteralInt 5]]
            results = evalManyToValue asts
            result = NE.last results
        result `shouldBe` Right (VInt 10)

      it "defines multiple functions" $ do
        let add = Lambda ["x"] (Call (VariableRef "+") [VariableRef "x", LiteralInt 1])
            mul = Lambda ["x"] (Call (VariableRef "*") [VariableRef "x", LiteralInt 2])
            define1 = Define "inc" add
            define2 = Define "double" mul
            (_, env) = evalMany [define1, define2]
        lookupEnv "inc" env `shouldBe` Just (VProcedure ["x"] (Call (VariableRef "+") [VariableRef "x", LiteralInt 1]))
        lookupEnv "double" env `shouldBe` Just (VProcedure ["x"] (Call (VariableRef "*") [VariableRef "x", LiteralInt 2]))

      it "defines a function with multiple parameters" $ do
        let lambda = Lambda ["x", "y"] (Call (VariableRef "+") [VariableRef "x", VariableRef "y"])
            ast = Define "add" lambda
            (_, env) = eval ast
        case lookupEnv "add" env of
          Just (VProcedure params _) -> params `shouldBe` ["x", "y"]
          _ -> expectationFailure "Expected VProcedure with two parameters"

      it "defines a recursive function structure" $ do
        let lambda =
              Lambda
                ["n"]
                ( If
                    (Call (VariableRef "eq?") [VariableRef "n", LiteralInt 0])
                    (LiteralInt 1)
                    (Call (VariableRef "*") [VariableRef "n", Call (VariableRef "factorial") [Call (VariableRef "-") [VariableRef "n", LiteralInt 1]]])
                )
            ast = Define "factorial" lambda
            (_, env) = eval ast
        case lookupEnv "factorial" env of
          Just (VProcedure _ _) -> True `shouldBe` True
          _ -> expectationFailure "Expected VProcedure"

      it "defines a function with no parameters" $ do
        let lambda = Lambda [] (LiteralInt 42)
            ast = Define "constant" lambda
            (_, env) = eval ast
        lookupEnv "constant" env `shouldBe` Just (VProcedure [] (LiteralInt 42))

-- | Test suite for lambda expressions
lambdaEvalSpec :: Spec
lambdaEvalSpec = do
  describe "Lambda" $ do
    it "creates a closure with single parameter" $ do
      let ast = Lambda ["x"] (VariableRef "x")
          result = evalToValue ast
      result `shouldBe` Right (VProcedure ["x"] (VariableRef "x"))

    it "creates a closure with multiple parameters" $ do
      let ast = Lambda ["x", "y"] (Call (VariableRef "+") [VariableRef "x", VariableRef "y"])
          result = evalToValue ast
      result `shouldBe` Right (VProcedure ["x", "y"] (Call (VariableRef "+") [VariableRef "x", VariableRef "y"]))

    it "creates a closure with no parameters" $ do
      let ast = Lambda [] (LiteralInt 42)
          result = evalToValue ast
      result `shouldBe` Right (VProcedure [] (LiteralInt 42))

    it "creates a closure with complex body" $ do
      let body =
            If
              (Call (VariableRef "<") [VariableRef "n", LiteralInt 0])
              (LiteralInt 0)
              (VariableRef "n")
          ast = Lambda ["n"] body
          result = evalToValue ast
      result `shouldBe` Right (VProcedure ["n"] body)

-- | Test suite for function call evaluation
callEvalSpec :: Spec
callEvalSpec = do
  describe "Function Calls" $ do
    it "calls a lambda with single argument" $ do
      let lambda = Lambda ["x"] (Call (VariableRef "+") [VariableRef "x", LiteralInt 1])
          ast = Call lambda [LiteralInt 5]
          result = evalToValue ast
      result `shouldBe` Right (VInt 6)

    it "calls a lambda with multiple arguments" $ do
      let lambda = Lambda ["x", "y"] (Call (VariableRef "*") [VariableRef "x", VariableRef "y"])
          ast = Call lambda [LiteralInt 3, LiteralInt 4]
      evalToValue ast `shouldBe` Right (VInt 12)

    it "calls a lambda with no arguments" $ do
      let lambda = Lambda [] (LiteralInt 42)
          ast = Call lambda []
      evalToValue ast `shouldBe` Right (VInt 42)

    it "errors on argument count mismatch (too few)" $ do
      let lambda = Lambda ["x", "y"] (VariableRef "x")
          ast = Call lambda [LiteralInt 5]
          result = evalToValue ast
      result `shouldSatisfy` isLeft

    it "errors on argument count mismatch (too many)" $ do
      let lambda = Lambda ["x"] (VariableRef "x")
          ast = Call lambda [LiteralInt 5, LiteralInt 10]
      evalToValue ast `shouldSatisfy` isLeft

    it "errors when calling non-function value" $ do
      let ast = Call (LiteralInt 42) [LiteralInt 1]
      evalToValue ast `shouldSatisfy` isLeft

-- Note: This test requires closure support (VProcedure capturing environment)
-- which the current AST design doesn't provide
-- it "calls nested lambdas" $ do
-- let outer = Lambda ["x"] (Lambda ["y"] (Call (VariableRef "+") [VariableRef "x", VariableRef "y"]))
-- inner = Call outer [LiteralInt 3]
-- ast = Call inner [LiteralInt 4]
-- result = evalToValue ast
-- result `shouldBe` Right (VInt 7)

-- | Test suite for built-in arithmetic operations
arithmeticBuiltinSpec :: Spec
arithmeticBuiltinSpec = do
  describe "Built-in Arithmetic" $ do
    describe "Addition" $ do
      it "adds two integers" $ do
        evalToValue (Call (VariableRef "+") [LiteralInt 1, LiteralInt 2]) `shouldBe` Right (VInt 3)

      it "adds two negative integers" $ do
        evalToValue (Call (VariableRef "+") [LiteralInt (-5), LiteralInt (-3)]) `shouldBe` Right (VInt (-8))

      it "adds positive and negative integers" $ do
        evalToValue (Call (VariableRef "+") [LiteralInt 10, LiteralInt (-3)]) `shouldBe` Right (VInt 7)

      it "errors on add with one argument" $ do
        evalToValue (Call (VariableRef "+") [LiteralInt 42]) `shouldSatisfy` isLeft

      it "errors on add with three (or more) arguments" $ do
        evalToValue (Call (VariableRef "+") [LiteralInt 1, LiteralInt 2, LiteralInt 3]) `shouldSatisfy` isLeft

      it "errors on add with no arguments" $ do
        evalToValue (Call (VariableRef "+") []) `shouldSatisfy` isLeft

      it "errors on add with non-integer" $ do
        evalToValue (Call (VariableRef "+") [LiteralInt 1, LiteralBool True]) `shouldSatisfy` isLeft

    describe "Subtraction" $ do
      it "subtracts two integers" $ do
        evalToValue (Call (VariableRef "-") [LiteralInt 10, LiteralInt 3]) `shouldBe` Right (VInt 7)

      it "subtracts with negative result" $ do
        evalToValue (Call (VariableRef "-") [LiteralInt 3, LiteralInt 10]) `shouldBe` Right (VInt (-7))

      it "subtracts negative integers" $ do
        evalToValue (Call (VariableRef "-") [LiteralInt (-5), LiteralInt (-3)]) `shouldBe` Right (VInt (-2))

      it "subtracts negative from positive" $ do
        evalToValue (Call (VariableRef "-") [LiteralInt 10, LiteralInt (-5)]) `shouldBe` Right (VInt 15)

      it "errors on subtract with one argument" $ do
        evalToValue (Call (VariableRef "-") [LiteralInt 5]) `shouldSatisfy` isLeft

      it "errors on subtract with three (or more) arguments" $ do
        evalToValue (Call (VariableRef "-") [LiteralInt 100, LiteralInt 20, LiteralInt 30]) `shouldSatisfy` isLeft

      it "errors on subtract with no arguments" $ do
        evalToValue (Call (VariableRef "-") []) `shouldSatisfy` isLeft

      it "errors on subtract with non-integer" $ do
        evalToValue (Call (VariableRef "-") [LiteralInt 10, LiteralBool False]) `shouldSatisfy` isLeft

    describe "Multiplication" $ do
      it "multiplies two integers" $ do
        evalToValue (Call (VariableRef "*") [LiteralInt 2, LiteralInt 3]) `shouldBe` Right (VInt 6)

      it "multiplies with zero" $ do
        evalToValue (Call (VariableRef "*") [LiteralInt 42, LiteralInt 0]) `shouldBe` Right (VInt 0)

      it "multiplies negative integers" $ do
        evalToValue (Call (VariableRef "*") [LiteralInt (-2), LiteralInt 3]) `shouldBe` Right (VInt (-6))

      it "multiplies two negative integers" $ do
        evalToValue (Call (VariableRef "*") [LiteralInt (-2), LiteralInt (-3)]) `shouldBe` Right (VInt 6)

      it "errors on multiply with one argument" $ do
        evalToValue (Call (VariableRef "*") [LiteralInt 7]) `shouldSatisfy` isLeft

      it "errors on multiply with three (or more) arguments" $ do
        evalToValue (Call (VariableRef "*") [LiteralInt 2, LiteralInt 3, LiteralInt 4]) `shouldSatisfy` isLeft

      it "errors on multiply with no arguments" $ do
        evalToValue (Call (VariableRef "*") []) `shouldSatisfy` isLeft

      it "errors on multiply with non-integer" $ do
        evalToValue (Call (VariableRef "*") [LiteralBool True, LiteralInt 3]) `shouldSatisfy` isLeft

    describe "Division" $ do
      it "divides two integers" $ do
        evalToValue (Call (VariableRef "div") [LiteralInt 10, LiteralInt 2]) `shouldBe` Right (VInt 5)

      it "divides with truncation" $ do
        evalToValue (Call (VariableRef "div") [LiteralInt 7, LiteralInt 2]) `shouldBe` Right (VInt 3)

      it "errors on division by zero" $ do
        evalToValue (Call (VariableRef "div") [LiteralInt 10, LiteralInt 0]) `shouldSatisfy` isLeft

      it "errors on division with one argument" $ do
        evalToValue (Call (VariableRef "div") [LiteralInt 10]) `shouldSatisfy` isLeft

      it "errors on division with three (or more) arguments" $ do
        evalToValue (Call (VariableRef "div") [LiteralInt 10, LiteralInt 2, LiteralInt 3]) `shouldSatisfy` isLeft

      it "errors on division with no arguments" $ do
        evalToValue (Call (VariableRef "div") []) `shouldSatisfy` isLeft

      it "errors on division with non-integer" $ do
        evalToValue (Call (VariableRef "div") [LiteralInt 10, LiteralBool False]) `shouldSatisfy` isLeft

    describe "Modulo" $ do
      it "calculates modulo" $ do
        evalToValue (Call (VariableRef "mod") [LiteralInt 10, LiteralInt 3]) `shouldBe` Right (VInt 1)

      it "calculates modulo with zero result" $ do
        evalToValue (Call (VariableRef "mod") [LiteralInt 10, LiteralInt 5]) `shouldBe` Right (VInt 0)

      it "errors on modulo by zero" $ do
        evalToValue (Call (VariableRef "mod") [LiteralInt 10, LiteralInt 0]) `shouldSatisfy` isLeft

      it "errors on modulo with one argument" $ do
        evalToValue (Call (VariableRef "mod") [LiteralInt 10]) `shouldSatisfy` isLeft

      it "errors on modulo with three (or more) arguments" $ do
        evalToValue (Call (VariableRef "mod") [LiteralInt 10, LiteralInt 3, LiteralInt 2]) `shouldSatisfy` isLeft

      it "errors on modulo with no arguments" $ do
        evalToValue (Call (VariableRef "mod") []) `shouldSatisfy` isLeft

      it "errors on modulo with non-integer" $ do
        evalToValue (Call (VariableRef "mod") [LiteralInt 10, LiteralBool False]) `shouldSatisfy` isLeft

-- | Test suite for built-in comparison operations
comparisonBuiltinSpec :: Spec
comparisonBuiltinSpec = do
  describe "Built-in Comparison" $ do
    describe "Equality" $ do
      it "checks equality for equal integers" $ do
        evalToValue (Call (VariableRef "eq?") [LiteralInt 5, LiteralInt 5]) `shouldBe` Right (VBool True)

      it "checks equality for different integers" $ do
        evalToValue (Call (VariableRef "eq?") [LiteralInt 5, LiteralInt 3]) `shouldBe` Right (VBool False)

      it "checks equality for equal booleans (True)" $ do
        evalToValue (Call (VariableRef "eq?") [LiteralBool True, LiteralBool True]) `shouldBe` Right (VBool True)

      it "checks equality for equal booleans (False)" $ do
        evalToValue (Call (VariableRef "eq?") [LiteralBool False, LiteralBool False]) `shouldBe` Right (VBool True)

      it "checks equality for different booleans" $ do
        evalToValue (Call (VariableRef "eq?") [LiteralBool True, LiteralBool False]) `shouldBe` Right (VBool False)

      it "errors on equality with one argument" $ do
        evalToValue (Call (VariableRef "eq?") [LiteralInt 5]) `shouldSatisfy` isLeft

      it "errors on equality with three (or more) arguments" $ do
        evalToValue (Call (VariableRef "eq?") [LiteralInt 5, LiteralInt 5, LiteralInt 5]) `shouldSatisfy` isLeft

      it "errors on equality with mixed types" $ do
        evalToValue (Call (VariableRef "eq?") [LiteralInt 5, LiteralBool True]) `shouldSatisfy` isLeft

    describe "Less-than" $ do
      it "checks less-than for true comparison" $ do
        evalToValue (Call (VariableRef "<") [LiteralInt 3, LiteralInt 5]) `shouldBe` Right (VBool True)

      it "checks less-than for false comparison" $ do
        evalToValue (Call (VariableRef "<") [LiteralInt 5, LiteralInt 3]) `shouldBe` Right (VBool False)

      it "checks less-than with equal values" $ do
        evalToValue (Call (VariableRef "<") [LiteralInt 5, LiteralInt 5]) `shouldBe` Right (VBool False)

      it "checks less-than with negative numbers" $ do
        evalToValue (Call (VariableRef "<") [LiteralInt (-5), LiteralInt 0]) `shouldBe` Right (VBool True)

      it "errors on less-than with one argument" $ do
        evalToValue (Call (VariableRef "<") [LiteralInt 5]) `shouldSatisfy` isLeft

      it "errors on less-than with non-integers" $ do
        evalToValue (Call (VariableRef "<") [LiteralBool True, LiteralBool False]) `shouldSatisfy` isLeft

-- | Test suite for conditional evaluation
conditionalEvalSpec :: Spec
conditionalEvalSpec = do
  describe "Conditionals" $ do
    it "evaluates then branch when condition is true" $ do
      evalToValue (If (LiteralBool True) (LiteralInt 1) (LiteralInt 2)) `shouldBe` Right (VInt 1)

    it "evaluates else branch when condition is false" $ do
      evalToValue (If (LiteralBool False) (LiteralInt 1) (LiteralInt 2)) `shouldBe` Right (VInt 2)

    it "evaluates condition expression" $ do
      let cond = Call (VariableRef "<") [LiteralInt 3, LiteralInt 5]
      evalToValue (If cond (LiteralInt 10) (LiteralInt 20)) `shouldBe` Right (VInt 10)

    it "evaluates else branch when condition evaluates to false" $ do
      let cond = Call (VariableRef "eq?") [LiteralInt 1, LiteralInt 2]
      evalToValue (If cond (LiteralInt 10) (LiteralInt 20)) `shouldBe` Right (VInt 20)

    it "errors on non-boolean condition" $ do
      evalToValue (If (LiteralInt 1) (LiteralInt 2) (LiteralInt 3)) `shouldSatisfy` isLeft

    it "errors when condition evaluates to non-boolean" $ do
      let cond = Call (VariableRef "+") [LiteralInt 1, LiteralInt 2]
      evalToValue (If cond (LiteralInt 10) (LiteralInt 20)) `shouldSatisfy` isLeft

-- | Test suite for complex expressions
complexExpressionSpec :: Spec
complexExpressionSpec = do
  describe "Complex Expressions" $ do
    it "evaluates nested arithmetic" $ do
      let ast = Call (VariableRef "+") [LiteralInt 1, Call (VariableRef "*") [LiteralInt 2, LiteralInt 3]]
      evalToValue ast `shouldBe` Right (VInt 7)

    it "evaluates deeply nested arithmetic" $ do
      let ast =
            Call
              (VariableRef "*")
              [ Call (VariableRef "+") [LiteralInt 2, LiteralInt 3],
                Call (VariableRef "-") [LiteralInt 10, LiteralInt 4]
              ]
      evalToValue ast `shouldBe` Right (VInt 30)

    it "evaluates lambda with computation" $ do
      let lambda = Lambda ["x"] (Call (VariableRef "*") [VariableRef "x", VariableRef "x"])
          ast = Call lambda [LiteralInt 5]
      evalToValue ast `shouldBe` Right (VInt 25)

    it "evaluates conditional inside arithmetic" $ do
      let ast =
            Call
              (VariableRef "+")
              [ If (LiteralBool True) (LiteralInt 10) (LiteralInt 20),
                LiteralInt 5
              ]
      evalToValue ast `shouldBe` Right (VInt 15)

    it "evaluates factorial-like expression" $ do
      let ast =
            If
              (Call (VariableRef "eq?") [LiteralInt 0, LiteralInt 0])
              (LiteralInt 1)
              (Call (VariableRef "*") [LiteralInt 5, LiteralInt 4])
      evalToValue ast `shouldBe` Right (VInt 1)

    it "evaluates define followed by use" $ do
      let defineAst = Define "double" (Lambda ["x"] (Call (VariableRef "*") [VariableRef "x", LiteralInt 2]))
          callAst = Call (VariableRef "double") [LiteralInt 7]
          result = NE.last $ evalManyToValue [defineAst, callAst]
      result `shouldBe` Right (VInt 14)

    it "evaluates multiple defines and uses" $ do
      let defineX = Define "x" (LiteralInt 10)
          defineY = Define "y" (LiteralInt 20)
          ast = Call (VariableRef "+") [VariableRef "x", VariableRef "y"]
          result = NE.last $ evalManyToValue [defineX, defineY, ast]
      result `shouldBe` Right (VInt 30)

-- | Exported combined spec for Evaluator
spec :: Spec
spec = do
  describe "Evaluator" $ do
    literalEvalSpec
    variableEvalSpec
    defineEvalSpec
    lambdaEvalSpec
    callEvalSpec
    arithmeticBuiltinSpec
    comparisonBuiltinSpec
    conditionalEvalSpec
    complexExpressionSpec
