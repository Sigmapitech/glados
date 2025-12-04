{-# LANGUAGE OverloadedStrings #-}

module ComplexExamplesSpec (spec) where

import Ast
import Evaluator
import SexprtoAST
import Test.Hspec

-- Helper to parse and evaluate
parseAndEval :: SExpr -> Either ErrorMsg Value
parseAndEval sexpr = case sexprToAst sexpr of
  Left err -> Left err
  Right ast -> fst $ evalWithEnv ast initialEnv

-- Helper to parse, convert, and evaluate in an existing environment
parseAndEvalWithEnv :: SExpr -> Environment -> EvalResult Value
parseAndEvalWithEnv sexpr env = case sexprToAst sexpr of
  Left err -> (Left err, env)
  Right ast -> evalWithEnv ast env

spec :: Spec
spec = do
  describe "Complex Lisp Examples" $ do
    describe "Factorial Function" $ do
      it "computes factorial of 5" $ do
        let defineFactorial =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "factorial",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "n"],
                      SList
                        [ SSymbol ifSymbol,
                          SList [SSymbol "eq?", SSymbol "n", SInteger 0],
                          SInteger 1,
                          SList [SSymbol "*", SSymbol "n", SList [SSymbol "factorial", SList [SSymbol "-", SSymbol "n", SInteger 1]]]
                        ]
                    ]
                ]
        let callFactorial = SList [SSymbol "factorial", SInteger 5]

        let (defineResult, env1) = parseAndEvalWithEnv defineFactorial initialEnv
        defineResult `shouldBe` Right VUnit

        let (result, _) = parseAndEvalWithEnv callFactorial env1
        result `shouldBe` Right (VInt 120)

    describe "Fibonacci Function" $ do
      it "computes fibonacci of 7" $ do
        let defineFib =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "fib",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "n"],
                      SList
                        [ SSymbol ifSymbol,
                          SList [SSymbol "<", SSymbol "n", SInteger 2],
                          SSymbol "n",
                          SList
                            [ SSymbol "+",
                              SList [SSymbol "fib", SList [SSymbol "-", SSymbol "n", SInteger 1]],
                              SList [SSymbol "fib", SList [SSymbol "-", SSymbol "n", SInteger 2]]
                            ]
                        ]
                    ]
                ]
        let callFib = SList [SSymbol "fib", SInteger 7]

        let (defineResult, env1) = parseAndEvalWithEnv defineFib initialEnv
        defineResult `shouldBe` Right VUnit

        let (result, _) = parseAndEvalWithEnv callFib env1
        result `shouldBe` Right (VInt 13)

    describe "Sum Range Function" $ do
      it "sums numbers from 1 to 10" $ do
        let defineSumRange =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "sum-range",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "a", SSymbol "b"],
                      SList
                        [ SSymbol ifSymbol,
                          SList [SSymbol "<", SSymbol "b", SSymbol "a"],
                          SInteger 0,
                          SList
                            [ SSymbol "+",
                              SSymbol "a",
                              SList [SSymbol "sum-range", SList [SSymbol "+", SSymbol "a", SInteger 1], SSymbol "b"]
                            ]
                        ]
                    ]
                ]
        let callSumRange = SList [SSymbol "sum-range", SInteger 1, SInteger 10]

        let (defineResult, env1) = parseAndEvalWithEnv defineSumRange initialEnv
        defineResult `shouldBe` Right VUnit

        let (result, _) = parseAndEvalWithEnv callSumRange env1
        result `shouldBe` Right (VInt 55)

    describe "Power Function" $ do
      it "computes 2^10" $ do
        let definePower =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "power",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "x", SSymbol "n"],
                      SList
                        [ SSymbol ifSymbol,
                          SList [SSymbol "eq?", SSymbol "n", SInteger 0],
                          SInteger 1,
                          SList
                            [ SSymbol ifSymbol,
                              SList [SSymbol "eq?", SSymbol "n", SInteger 1],
                              SSymbol "x",
                              SList [SSymbol "*", SSymbol "x", SList [SSymbol "power", SSymbol "x", SList [SSymbol "-", SSymbol "n", SInteger 1]]]
                            ]
                        ]
                    ]
                ]
        let callPower = SList [SSymbol "power", SInteger 2, SInteger 10]

        let (defineResult, env1) = parseAndEvalWithEnv definePower initialEnv
        defineResult `shouldBe` Right VUnit

        let (result, _) = parseAndEvalWithEnv callPower env1
        result `shouldBe` Right (VInt 1024)

    describe "GCD Function" $ do
      it "computes GCD of 48 and 18" $ do
        let defineGcd =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "gcd",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "a", SSymbol "b"],
                      SList
                        [ SSymbol ifSymbol,
                          SList [SSymbol "eq?", SSymbol "b", SInteger 0],
                          SSymbol "a",
                          SList [SSymbol "gcd", SSymbol "b", SList [SSymbol "mod", SSymbol "a", SSymbol "b"]]
                        ]
                    ]
                ]
        let callGcd = SList [SSymbol "gcd", SInteger 48, SInteger 18]

        let (defineResult, env1) = parseAndEvalWithEnv defineGcd initialEnv
        defineResult `shouldBe` Right VUnit

        let (result, _) = parseAndEvalWithEnv callGcd env1
        result `shouldBe` Right (VInt 6)

    describe "Higher-Order Functions" $ do
      it "applies a function twice using higher-order function" $ do
        let defineTwice =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "twice",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "f", SSymbol "x"],
                      SList [SSymbol "f", SList [SSymbol "f", SSymbol "x"]]
                    ]
                ]
        let defineDouble =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "double",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "n"],
                      SList [SSymbol "*", SSymbol "n", SInteger 2]
                    ]
                ]
        let callTwice = SList [SSymbol "twice", SSymbol "double", SInteger 5]

        let (_, env1) = parseAndEvalWithEnv defineTwice initialEnv
        let (_, env2) = parseAndEvalWithEnv defineDouble env1
        let (result, _) = parseAndEvalWithEnv callTwice env2
        result `shouldBe` Right (VInt 20)

    describe "Complex Nested Calculations" $ do
      it "evaluates deeply nested arithmetic" $ do
        let expr =
              SList
                [ SSymbol "+",
                  SList [SSymbol "*", SInteger 2, SList [SSymbol "+", SInteger 3, SInteger 4]],
                  SList [SSymbol "-", SInteger 10, SList [SSymbol "div", SInteger 20, SInteger 4]]
                ]
        parseAndEval expr `shouldBe` Right (VInt 19)

      it "evaluates nested conditionals" $ do
        let defineComplexCalc =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "complex-calc",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "x", SSymbol "y", SSymbol "z"],
                      SList
                        [ SSymbol ifSymbol,
                          SList [SSymbol "<", SSymbol "x", SSymbol "y"],
                          SList
                            [ SSymbol ifSymbol,
                              SList [SSymbol "<", SSymbol "y", SSymbol "z"],
                              SList [SSymbol "*", SSymbol "x", SList [SSymbol "+", SSymbol "y", SSymbol "z"]],
                              SList [SSymbol "-", SList [SSymbol "*", SSymbol "x", SSymbol "y"], SSymbol "z"]
                            ],
                          SList
                            [ SSymbol ifSymbol,
                              SList [SSymbol "<", SSymbol "x", SSymbol "z"],
                              SList [SSymbol "+", SList [SSymbol "*", SSymbol "x", SSymbol "y"], SList [SSymbol "*", SSymbol "y", SSymbol "z"]],
                              SList [SSymbol "-", SList [SSymbol "+", SSymbol "x", SSymbol "y"], SSymbol "z"]
                            ]
                        ]
                    ]
                ]
        let callComplexCalc = SList [SSymbol "complex-calc", SInteger 5, SInteger 10, SInteger 3]

        let (_, env1) = parseAndEvalWithEnv defineComplexCalc initialEnv
        let (result, _) = parseAndEvalWithEnv callComplexCalc env1
        result `shouldBe` Right (VInt 47)

    describe "Mathematical Functions" $ do
      it "computes sum of squares" $ do
        let defineSquare =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "square",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "n"],
                      SList [SSymbol "*", SSymbol "n", SSymbol "n"]
                    ]
                ]
        let defineSumOfSquares =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "sum-of-squares",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "a", SSymbol "b"],
                      SList [SSymbol "+", SList [SSymbol "square", SSymbol "a"], SList [SSymbol "square", SSymbol "b"]]
                    ]
                ]
        let callSumOfSquares = SList [SSymbol "sum-of-squares", SInteger 3, SInteger 4]

        let (_, env1) = parseAndEvalWithEnv defineSquare initialEnv
        let (_, env2) = parseAndEvalWithEnv defineSumOfSquares env1
        let (result, _) = parseAndEvalWithEnv callSumOfSquares env2
        result `shouldBe` Right (VInt 25)

    describe "Closure Behavior" $ do
      it "captures environment in nested lambdas" $ do
        let defineMakeAdder =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "make-adder",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "x"],
                      SList
                        [ SSymbol lambdaSymbol,
                          SList [SSymbol "y"],
                          SList [SSymbol "+", SSymbol "x", SSymbol "y"]
                        ]
                    ]
                ]
        let defineAdd5 = SList [SSymbol defineSymbol, SSymbol "add5", SList [SSymbol "make-adder", SInteger 5]]
        let callAdd5 = SList [SSymbol "add5", SInteger 10]

        let (_, env1) = parseAndEvalWithEnv defineMakeAdder initialEnv
        let (_, env2) = parseAndEvalWithEnv defineAdd5 env1
        let (result, _) = parseAndEvalWithEnv callAdd5 env2
        result `shouldBe` Right (VInt 15)

    describe "Ackermann Function" $ do
      it "computes Ackermann(2, 2)" $ do
        let defineAckermann =
              SList
                [ SSymbol defineSymbol,
                  SSymbol "ackermann",
                  SList
                    [ SSymbol lambdaSymbol,
                      SList [SSymbol "m", SSymbol "n"],
                      SList
                        [ SSymbol ifSymbol,
                          SList [SSymbol "eq?", SSymbol "m", SInteger 0],
                          SList [SSymbol "+", SSymbol "n", SInteger 1],
                          SList
                            [ SSymbol ifSymbol,
                              SList [SSymbol "eq?", SSymbol "n", SInteger 0],
                              SList [SSymbol "ackermann", SList [SSymbol "-", SSymbol "m", SInteger 1], SInteger 1],
                              SList
                                [ SSymbol "ackermann",
                                  SList [SSymbol "-", SSymbol "m", SInteger 1],
                                  SList [SSymbol "ackermann", SSymbol "m", SList [SSymbol "-", SSymbol "n", SInteger 1]]
                                ]
                            ]
                        ]
                    ]
                ]
        let callAckermann = SList [SSymbol "ackermann", SInteger 2, SInteger 2]

        let (_, env1) = parseAndEvalWithEnv defineAckermann initialEnv
        let (result, _) = parseAndEvalWithEnv callAckermann env1
        result `shouldBe` Right (VInt 7)
