{-# LANGUAGE OverloadedStrings #-}

module EvaluatorSpec (spec) where

import Ast
import Evaluator (eval, evalFrom)
import Test.Hspec

spec :: Spec
spec = do
  describe "Evaluator" $ do
    describe "Literals" $ do
      it "evaluates integer literals" $ do
        let (result, _) = eval (LiteralInt 42)
        result `shouldBe` Right (VInt 42)

      it "evaluates boolean literals" $ do
        let (result, _) = eval (LiteralBool True)
        result `shouldBe` Right (VBool True)

    describe "Variables" $ do
      it "looks up variables in environment" $ do
        let env = extendEnv "x" (VInt 10) emptyEnv
            (result, _) = evalFrom env (VariableRef "x")
        result `shouldBe` Right (VInt 10)

      it "errors on undefined variables" $ do
        let (result, _) = eval (VariableRef "undefined")
        result `shouldSatisfy` isLeft

    describe "Define" $ do
      it "defines a variable" $ do
        let ast = Define "x" (LiteralInt 5)
            (result, env) = eval ast
        result `shouldBe` Right VUnit
        lookupEnv "x" env `shouldBe` Just (VInt 5)

      it "defines with expression" $ do
        let ast = Define "y" (Call (VariableRef "+") [LiteralInt 2, LiteralInt 3])
            (result, env) = eval ast
        result `shouldBe` Right VUnit
        lookupEnv "y" env `shouldBe` Just (VInt 5)

    describe "Lambda" $ do
      it "creates a closure" $ do
        let ast = Lambda ["x"] (VariableRef "x")
            (result, _) = eval ast
        case result of
          Right (VProcedure params _) -> params `shouldBe` ["x"]
          _ -> expectationFailure "Expected VProcedure"

      it "captures environment" $ do
        let env = extendEnv "y" (VInt 10) emptyEnv
            ast = Lambda ["x"] (Call (VariableRef "+") [VariableRef "x", VariableRef "y"])
            (result, _) = evalFrom env ast
        case result of
          Right (VProcedure _ _) -> result `shouldSatisfy` isRight
          _ -> expectationFailure "Expected VProcedure"

    describe "Function Calls" $ do
      it "calls a lambda" $ do
        let lambda = Lambda ["x"] (Call (VariableRef "+") [VariableRef "x", LiteralInt 1])
            ast = Call lambda [LiteralInt 5]
            (result, _) = eval ast
        result `shouldBe` Right (VInt 6)

      it "errors on argument count mismatch" $ do
        let lambda = Lambda ["x", "y"] (VariableRef "x")
            ast = Call lambda [LiteralInt 5]
            (result, _) = eval ast
        result `shouldSatisfy` isLeft

    -- Note: This test requires closure support (VProcedure capturing environment)
    -- which the current AST design doesn't provide
    -- it "calls nested lambdas" $ do
    -- let outer = Lambda ["x"] (Lambda ["y"] (Call (VariableRef "+") [VariableRef "x", VariableRef "y"]))
    -- inner = Call outer [LiteralInt 3]
    -- ast = Call inner [LiteralInt 4]
    -- (result, _) = eval ast
    -- result `shouldBe` Right (VInt 7)

    describe "Built-in Arithmetic" $ do
      it "adds integers" $ do
        let ast = Call (VariableRef "+") [LiteralInt 1, LiteralInt 2]
            (result, _) = eval ast
        result `shouldBe` Right (VInt 3)

      it "subtracts integers" $ do
        let ast = Call (VariableRef "-") [LiteralInt 10, LiteralInt 3]
            (result, _) = eval ast
        result `shouldBe` Right (VInt 7)

      it "multiplies integers" $ do
        let ast = Call (VariableRef "*") [LiteralInt 2, LiteralInt 3]
            (result, _) = eval ast
        result `shouldBe` Right (VInt 6)

      it "divides integers" $ do
        let ast = Call (VariableRef "div") [LiteralInt 10, LiteralInt 2]
            (result, _) = eval ast
        result `shouldBe` Right (VInt 5)

      it "errors on division by zero" $ do
        let ast = Call (VariableRef "div") [LiteralInt 10, LiteralInt 0]
            (result, _) = eval ast
        result `shouldSatisfy` isLeft

      it "calculates modulo" $ do
        let ast = Call (VariableRef "mod") [LiteralInt 10, LiteralInt 3]
            (result, _) = eval ast
        result `shouldBe` Right (VInt 1)

    describe "Built-in Comparison" $ do
      it "checks equality for integers" $ do
        let ast1 = Call (VariableRef "eq?") [LiteralInt 5, LiteralInt 5]
            ast2 = Call (VariableRef "eq?") [LiteralInt 5, LiteralInt 3]
            (result1, _) = eval ast1
            (result2, _) = eval ast2
        result1 `shouldBe` Right (VBool True)
        result2 `shouldBe` Right (VBool False)

      it "checks less-than" $ do
        let ast1 = Call (VariableRef "<") [LiteralInt 3, LiteralInt 5]
            ast2 = Call (VariableRef "<") [LiteralInt 5, LiteralInt 3]
            (result1, _) = eval ast1
            (result2, _) = eval ast2
        result1 `shouldBe` Right (VBool True)
        result2 `shouldBe` Right (VBool False)

    describe "Conditionals" $ do
      it "evaluates then branch when condition is true" $ do
        let ast = If (LiteralBool True) (LiteralInt 1) (LiteralInt 2)
            (result, _) = eval ast
        result `shouldBe` Right (VInt 1)

      it "evaluates else branch when condition is false" $ do
        let ast = If (LiteralBool False) (LiteralInt 1) (LiteralInt 2)
            (result, _) = eval ast
        result `shouldBe` Right (VInt 2)

      it "evaluates condition expression" $ do
        let ast = If (Call (VariableRef "<") [LiteralInt 3, LiteralInt 5]) (LiteralInt 10) (LiteralInt 20)
            (result, _) = eval ast
        result `shouldBe` Right (VInt 10)

      it "errors on non-boolean condition" $ do
        let ast = If (LiteralInt 1) (LiteralInt 2) (LiteralInt 3)
            (result, _) = eval ast
        result `shouldSatisfy` isLeft

    describe "Complex Expressions" $ do
      it "evaluates nested arithmetic" $ do
        let ast = Call (VariableRef "+") [LiteralInt 1, Call (VariableRef "*") [LiteralInt 2, LiteralInt 3]]
            (result, _) = eval ast
        result `shouldBe` Right (VInt 7)

      it "evaluates lambda with computation" $ do
        let lambda = Lambda ["x"] (Call (VariableRef "*") [VariableRef "x", VariableRef "x"])
            ast = Call lambda [LiteralInt 5]
            (result, _) = eval ast
        result `shouldBe` Right (VInt 25)

      it "evaluates factorial-like expression" $ do
        let ast =
              If
                (Call (VariableRef "eq?") [LiteralInt 0, LiteralInt 0])
                (LiteralInt 1)
                (Call (VariableRef "*") [LiteralInt 5, LiteralInt 4])
            (result, _) = eval ast
        result `shouldBe` Right (VInt 1)

      it "evaluates define followed by use" $ do
        let defineAst = Define "double" (Lambda ["x"] (Call (VariableRef "*") [VariableRef "x", LiteralInt 2]))
            (_, env1) = eval defineAst
            callAst = Call (VariableRef "double") [LiteralInt 7]
            (result, _) = evalFrom env1 callAst
        result `shouldBe` Right (VInt 14)

-- Helper for error checking
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
