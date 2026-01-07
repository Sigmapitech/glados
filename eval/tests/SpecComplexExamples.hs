{-# LANGUAGE OverloadedStrings #-}

module SpecComplexExamples (specComplexExamples) where

import AST.Lisp.AST
import qualified Data.List.NonEmpty as NE
import Evaluator (evalManyToValue, evalToValue)
import Test.Hspec

specComplexExamples :: Spec
specComplexExamples = do
  describe "Complex Lisp Examples" $ do
    describe "Factorial Function" $ do
      it "computes factorial of 5" $ do
        let defineFactorial =
              Define "factorial" $
                Lambda ["n"] $
                  If
                    (Call (VariableRef "eq?") [VariableRef "n", LiteralInt 0])
                    (LiteralInt 1)
                    (Call (VariableRef "*") [VariableRef "n", Call (VariableRef "factorial") [Call (VariableRef "-") [VariableRef "n", LiteralInt 1]]])
            callFactorial = Call (VariableRef "factorial") [LiteralInt 5]
            results = evalManyToValue [defineFactorial, callFactorial]
        NE.last results `shouldBe` Right (VInt 120)

    describe "Fibonacci Function" $ do
      it "computes fibonacci of 7" $ do
        let defineFib =
              Define "fib" $
                Lambda ["n"] $
                  If
                    (Call (VariableRef "<") [VariableRef "n", LiteralInt 2])
                    (VariableRef "n")
                    (Call (VariableRef "+") [Call (VariableRef "fib") [Call (VariableRef "-") [VariableRef "n", LiteralInt 1]], Call (VariableRef "fib") [Call (VariableRef "-") [VariableRef "n", LiteralInt 2]]])
            callFib = Call (VariableRef "fib") [LiteralInt 7]
            results = evalManyToValue [defineFib, callFib]
        NE.last results `shouldBe` Right (VInt 13)

    describe "Sum Range Function" $ do
      it "sums numbers from 1 to 10" $ do
        let defineSumRange =
              Define "sum-range" $
                Lambda ["a", "b"] $
                  If
                    (Call (VariableRef "<") [VariableRef "b", VariableRef "a"])
                    (LiteralInt 0)
                    (Call (VariableRef "+") [VariableRef "a", Call (VariableRef "sum-range") [Call (VariableRef "+") [VariableRef "a", LiteralInt 1], VariableRef "b"]])
            callSumRange = Call (VariableRef "sum-range") [LiteralInt 1, LiteralInt 10]
            results = evalManyToValue [defineSumRange, callSumRange]
        NE.last results `shouldBe` Right (VInt 55)

    describe "Power Function" $ do
      it "computes 2^10" $ do
        let definePower =
              Define "power" $
                Lambda ["x", "n"] $
                  If
                    (Call (VariableRef "eq?") [VariableRef "n", LiteralInt 0])
                    (LiteralInt 1)
                    ( If
                        (Call (VariableRef "eq?") [VariableRef "n", LiteralInt 1])
                        (VariableRef "x")
                        (Call (VariableRef "*") [VariableRef "x", Call (VariableRef "power") [VariableRef "x", Call (VariableRef "-") [VariableRef "n", LiteralInt 1]]])
                    )
            callPower = Call (VariableRef "power") [LiteralInt 2, LiteralInt 10]
            results = evalManyToValue [definePower, callPower]
        NE.last results `shouldBe` Right (VInt 1024)

    describe "GCD Function" $ do
      it "computes GCD of 48 and 18" $ do
        let defineGcd =
              Define "gcd" $
                Lambda ["a", "b"] $
                  If
                    (Call (VariableRef "eq?") [VariableRef "b", LiteralInt 0])
                    (VariableRef "a")
                    (Call (VariableRef "gcd") [VariableRef "b", Call (VariableRef "mod") [VariableRef "a", VariableRef "b"]])
            callGcd = Call (VariableRef "gcd") [LiteralInt 48, LiteralInt 18]
            results = evalManyToValue [defineGcd, callGcd]
        NE.last results `shouldBe` Right (VInt 6)

    describe "Higher-Order Functions" $ do
      it "applies a function twice using higher-order function" $ do
        let defineTwice =
              Define "twice" $
                Lambda ["f", "x"] $
                  Call (VariableRef "f") [Call (VariableRef "f") [VariableRef "x"]]
            defineDouble =
              Define "double" $
                Lambda ["n"] $
                  Call (VariableRef "*") [VariableRef "n", LiteralInt 2]
            callTwice = Call (VariableRef "twice") [VariableRef "double", LiteralInt 5]
            results = evalManyToValue [defineTwice, defineDouble, callTwice]
        NE.last results `shouldBe` Right (VInt 20)

    describe "Complex Nested Calculations" $ do
      it "evaluates deeply nested arithmetic" $ do
        let expr =
              Call
                (VariableRef "+")
                [ Call (VariableRef "*") [LiteralInt 2, Call (VariableRef "+") [LiteralInt 3, LiteralInt 4]],
                  Call (VariableRef "-") [LiteralInt 10, Call (VariableRef "div") [LiteralInt 20, LiteralInt 4]]
                ]
        evalToValue expr `shouldBe` Right (VInt 19)

      it "evaluates nested conditionals" $ do
        let defineComplexCalc =
              Define "complex-calc" $
                Lambda ["x", "y", "z"] $
                  If
                    (Call (VariableRef "<") [VariableRef "x", VariableRef "y"])
                    ( If
                        (Call (VariableRef "<") [VariableRef "y", VariableRef "z"])
                        (Call (VariableRef "*") [VariableRef "x", Call (VariableRef "+") [VariableRef "y", VariableRef "z"]])
                        (Call (VariableRef "-") [Call (VariableRef "*") [VariableRef "x", VariableRef "y"], VariableRef "z"])
                    )
                    ( If
                        (Call (VariableRef "<") [VariableRef "x", VariableRef "z"])
                        (Call (VariableRef "+") [Call (VariableRef "*") [VariableRef "x", VariableRef "y"], Call (VariableRef "*") [VariableRef "y", VariableRef "z"]])
                        (Call (VariableRef "-") [Call (VariableRef "+") [VariableRef "x", VariableRef "y"], VariableRef "z"])
                    )
            callComplexCalc = Call (VariableRef "complex-calc") [LiteralInt 5, LiteralInt 10, LiteralInt 3]
            results = evalManyToValue [defineComplexCalc, callComplexCalc]
        NE.last results `shouldBe` Right (VInt 47)

    describe "Mathematical Functions" $ do
      it "computes sum of squares" $ do
        let defineSquare =
              Define "square" $
                Lambda ["n"] $
                  Call (VariableRef "*") [VariableRef "n", VariableRef "n"]
            defineSumOfSquares =
              Define "sum-of-squares" $
                Lambda ["a", "b"] $
                  Call (VariableRef "+") [Call (VariableRef "square") [VariableRef "a"], Call (VariableRef "square") [VariableRef "b"]]
            callSumOfSquares = Call (VariableRef "sum-of-squares") [LiteralInt 3, LiteralInt 4]
            results = evalManyToValue [defineSquare, defineSumOfSquares, callSumOfSquares]
        NE.last results `shouldBe` Right (VInt 25)

    -- Note: This test requires closure support (VProcedure capturing environment)
    -- which the current AST design doesn't provide
    -- describe "Closure Behavior" $ do
    -- it "captures environment in nested lambdas" $ do
    -- let defineMakeAdder =
    -- SList
    -- [ SSymbol defineSymbol,
    -- SSymbol "make-adder",
    -- SList
    -- [ SSymbol lambdaSymbol,
    -- SList [SSymbol "x"],
    -- SList
    -- [ SSymbol lambdaSymbol,
    -- SList [SSymbol "y"],
    -- SList [SSymbol "+", SSymbol "x", SSymbol "y"]
    -- ]
    -- ]
    -- ]
    -- let defineAdd5 = SList [SSymbol defineSymbol, SSymbol "add5", SList [SSymbol "make-adder", SInteger 5]]
    -- let callAdd5 = SList [SSymbol "add5", SInteger 10]
    --
    -- let (_, env1) = parseAndEvalWithEnv defineMakeAdder initialEnv
    -- let (_, env2) = parseAndEvalWithEnv defineAdd5 env1
    -- let (result, _) = parseAndEvalWithEnv callAdd5 env2
    -- result `shouldBe` Right (VInt 15)

    describe "Ackermann Function" $ do
      it "computes Ackermann(2, 2)" $ do
        let defineAckermann =
              Define "ackermann" $
                Lambda ["m", "n"] $
                  If
                    (Call (VariableRef "eq?") [VariableRef "m", LiteralInt 0])
                    (Call (VariableRef "+") [VariableRef "n", LiteralInt 1])
                    ( If
                        (Call (VariableRef "eq?") [VariableRef "n", LiteralInt 0])
                        (Call (VariableRef "ackermann") [Call (VariableRef "-") [VariableRef "m", LiteralInt 1], LiteralInt 1])
                        (Call (VariableRef "ackermann") [Call (VariableRef "-") [VariableRef "m", LiteralInt 1], Call (VariableRef "ackermann") [VariableRef "m", Call (VariableRef "-") [VariableRef "n", LiteralInt 1]]])
                    )
            callAckermann = Call (VariableRef "ackermann") [LiteralInt 2, LiteralInt 2]
            results = evalManyToValue [defineAckermann, callAckermann]
        NE.last results `shouldBe` Right (VInt 7)
