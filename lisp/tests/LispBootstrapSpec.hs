{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- LispBootstrapSpec
-}

module LispBootstrapSpec (bootstrapModuleSpec) where

import Bootstrap
import Test.Hspec

-- | Test suite for SExpr data type and getters
sexprSpec :: Spec
sexprSpec = do
  describe "SExpr constructors" $ do
    it "should create SInteger" $ do
      let expr = SInteger 42
      getInteger expr `shouldBe` Just 42

    it "should create SSymbol" $ do
      let expr = SSymbol "test"
      getSymbol expr `shouldBe` Just "test"

    it "should create SList" $ do
      let expr = SList [SInteger 1, SInteger 2]
      getList expr `shouldBe` Just [SInteger 1, SInteger 2]

  describe "getInteger" $ do
    it "should extract integer from SInteger" $ do
      getInteger (SInteger 42) `shouldBe` Just 42
      getInteger (SInteger 0) `shouldBe` Just 0
      getInteger (SInteger (-100)) `shouldBe` Just (-100)

    it "should return Nothing for non-integer SExpr" $ do
      getInteger (SSymbol "x") `shouldBe` Nothing
      getInteger (SList []) `shouldBe` Nothing

  describe "getSymbol" $ do
    it "should extract symbol from SSymbol" $ do
      getSymbol (SSymbol "x") `shouldBe` Just "x"
      getSymbol (SSymbol "+") `shouldBe` Just "+"
      getSymbol (SSymbol "define") `shouldBe` Just "define"

    it "should return Nothing for non-symbol SExpr" $ do
      getSymbol (SInteger 42) `shouldBe` Nothing
      getSymbol (SList []) `shouldBe` Nothing

  describe "getList" $ do
    it "should extract list from SList" $ do
      getList (SList []) `shouldBe` Just []
      getList (SList [SInteger 1]) `shouldBe` Just [SInteger 1]
      getList (SList [SSymbol "+", SInteger 1, SInteger 2]) `shouldBe` Just [SSymbol "+", SInteger 1, SInteger 2]

    it "should return Nothing for non-list SExpr" $ do
      getList (SInteger 42) `shouldBe` Nothing
      getList (SSymbol "x") `shouldBe` Nothing

-- | Test suite for printTree function
printTreeSpec :: Spec
printTreeSpec = do
  describe "printTree" $ do
    it "should print integers" $ do
      printTree (SInteger 42) `shouldBe` Just "42"
      printTree (SInteger 0) `shouldBe` Just "0"
      printTree (SInteger (-5)) `shouldBe` Just "-5"

    it "should print symbols" $ do
      printTree (SSymbol "x") `shouldBe` Just "x"
      printTree (SSymbol "+") `shouldBe` Just "+"
      printTree (SSymbol "define") `shouldBe` Just "define"

    it "should print empty list" $ do
      printTree (SList []) `shouldBe` Just "()"

    it "should print simple list" $ do
      printTree (SList [SSymbol "+", SInteger 1, SInteger 2]) `shouldBe` Just "(+ 1 2)"

    it "should print nested list" $ do
      printTree (SList [SSymbol "+", SList [SSymbol "*", SInteger 2, SInteger 3], SInteger 1])
        `shouldBe` Just "(+ (* 2 3) 1)"

-- | Test suite for AST data type
astDataSpec :: Spec
astDataSpec = do
  describe "AST constructors" $ do
    it "should create ASTInteger with intValue accessor" $ do
      intValue (ASTInteger 42) `shouldBe` 42

    it "should create ASTSymbol with symbolName accessor" $ do
      symbolName (ASTSymbol "x") `shouldBe` "x"

    it "should create ASTBoolean with boolValue accessor" $ do
      boolValue (ASTBoolean True) `shouldBe` True
      boolValue (ASTBoolean False) `shouldBe` False

    it "should create Define with defName and defValue accessors" $ do
      let def = Define "x" (ASTInteger 5)
      defName def `shouldBe` "x"
      defValue def `shouldBe` ASTInteger 5

    it "should create Call with callFunction and callArgs accessors" $ do
      let call = Call "+" [ASTInteger 1, ASTInteger 2]
      callFunction call `shouldBe` "+"
      callArgs call `shouldBe` [ASTInteger 1, ASTInteger 2]

-- | Test suite for sexprToAST conversion
sexprToASTSpec :: Spec
sexprToASTSpec = do
  describe "sexprToAST" $ do
    it "should convert SInteger to ASTInteger" $ do
      sexprToAST (SInteger 42) `shouldBe` Just (ASTInteger 42)
      sexprToAST (SInteger (-5)) `shouldBe` Just (ASTInteger (-5))

    it "should convert SSymbol to ASTSymbol" $ do
      sexprToAST (SSymbol "x") `shouldBe` Just (ASTSymbol "x")
      sexprToAST (SSymbol "+") `shouldBe` Just (ASTSymbol "+")

    it "should convert define expression" $ do
      let sexpr = SList [SSymbol "define", SSymbol "x", SInteger 5]
      sexprToAST sexpr `shouldBe` Just (Define "x" (ASTInteger 5))

    it "should convert define with nested expression" $ do
      let sexpr = SList [SSymbol "define", SSymbol "y", SList [SSymbol "+", SInteger 1, SInteger 2]]
      sexprToAST sexpr `shouldBe` Just (Define "y" (Call "+" [ASTInteger 1, ASTInteger 2]))

    it "should return Nothing for invalid define (missing value)" $ do
      let sexpr = SList [SSymbol "define", SSymbol "x"]
      sexprToAST sexpr `shouldBe` Nothing

    it "should return Nothing for invalid define (non-symbol name)" $ do
      let sexpr = SList [SSymbol "define", SInteger 42, SInteger 5]
      sexprToAST sexpr `shouldBe` Nothing

    it "should convert function call" $ do
      let sexpr = SList [SSymbol "+", SInteger 1, SInteger 2]
      sexprToAST sexpr `shouldBe` Just (Call "+" [ASTInteger 1, ASTInteger 2])

    it "should convert nested function call" $ do
      let sexpr = SList [SSymbol "+", SList [SSymbol "*", SInteger 2, SInteger 3], SInteger 1]
      sexprToAST sexpr `shouldBe` Just (Call "+" [Call "*" [ASTInteger 2, ASTInteger 3], ASTInteger 1])

    it "should convert function call with no arguments" $ do
      let sexpr = SList [SSymbol "foo"]
      sexprToAST sexpr `shouldBe` Just (Call "foo" [])

    it "should return Nothing for empty list" $ do
      sexprToAST (SList []) `shouldBe` Nothing

    it "should return Nothing for list starting with non-symbol" $ do
      sexprToAST (SList [SInteger 1, SInteger 2]) `shouldBe` Nothing

-- | Test suite for evalAST function
evalASTSpec :: Spec
evalASTSpec = do
  describe "evalAST" $ do
    describe "literal evaluation" $ do
      it "should evaluate ASTInteger to itself" $ do
        evalAST (ASTInteger 42) `shouldBe` Just (ASTInteger 42)

      it "should evaluate ASTBoolean to itself" $ do
        evalAST (ASTBoolean True) `shouldBe` Just (ASTBoolean True)
        evalAST (ASTBoolean False) `shouldBe` Just (ASTBoolean False)

      it "should evaluate ASTSymbol to itself" $ do
        evalAST (ASTSymbol "x") `shouldBe` Just (ASTSymbol "x")

    describe "arithmetic operations" $ do
      it "should evaluate addition" $ do
        evalAST (Call "+" [ASTInteger 1, ASTInteger 2]) `shouldBe` Just (ASTInteger 3)
        evalAST (Call "+" [ASTInteger 0, ASTInteger 0]) `shouldBe` Just (ASTInteger 0)
        evalAST (Call "+" [ASTInteger (-5), ASTInteger 3]) `shouldBe` Just (ASTInteger (-2))

      it "should evaluate subtraction" $ do
        evalAST (Call "-" [ASTInteger 5, ASTInteger 3]) `shouldBe` Just (ASTInteger 2)
        evalAST (Call "-" [ASTInteger 3, ASTInteger 5]) `shouldBe` Just (ASTInteger (-2))

      it "should evaluate multiplication" $ do
        evalAST (Call "*" [ASTInteger 3, ASTInteger 4]) `shouldBe` Just (ASTInteger 12)
        evalAST (Call "*" [ASTInteger 0, ASTInteger 100]) `shouldBe` Just (ASTInteger 0)
        evalAST (Call "*" [ASTInteger (-2), ASTInteger 3]) `shouldBe` Just (ASTInteger (-6))

      it "should evaluate division" $ do
        evalAST (Call "/" [ASTInteger 10, ASTInteger 2]) `shouldBe` Just (ASTInteger 5)
        evalAST (Call "/" [ASTInteger 7, ASTInteger 2]) `shouldBe` Just (ASTInteger 3)

      it "should return Nothing for division by zero" $ do
        evalAST (Call "/" [ASTInteger 10, ASTInteger 0]) `shouldBe` Nothing

      it "should evaluate modulo" $ do
        evalAST (Call "mod" [ASTInteger 10, ASTInteger 3]) `shouldBe` Just (ASTInteger 1)
        evalAST (Call "mod" [ASTInteger 9, ASTInteger 3]) `shouldBe` Just (ASTInteger 0)

      it "should return Nothing for modulo by zero" $ do
        evalAST (Call "mod" [ASTInteger 10, ASTInteger 0]) `shouldBe` Nothing

    describe "nested evaluation" $ do
      it "should evaluate nested arithmetic expressions" $ do
        -- (+ (* 2 3) 1) = 7
        evalAST (Call "+" [Call "*" [ASTInteger 2, ASTInteger 3], ASTInteger 1])
          `shouldBe` Just (ASTInteger 7)

      it "should evaluate deeply nested expressions" $ do
        -- (+ (- 10 3) (* 2 2)) = 7 + 4 = 11
        evalAST (Call "+" [Call "-" [ASTInteger 10, ASTInteger 3], Call "*" [ASTInteger 2, ASTInteger 2]])
          `shouldBe` Just (ASTInteger 11)

    describe "define evaluation" $ do
      it "should evaluate define with simple value" $ do
        evalAST (Define "x" (ASTInteger 5)) `shouldBe` Just (Define "x" (ASTInteger 5))

      it "should evaluate define with expression" $ do
        evalAST (Define "y" (Call "+" [ASTInteger 1, ASTInteger 2]))
          `shouldBe` Just (Define "y" (ASTInteger 3))

    describe "unknown function calls" $ do
      it "should preserve unknown function calls with evaluated arguments" $ do
        evalAST (Call "foo" [ASTInteger 1, Call "+" [ASTInteger 2, ASTInteger 3]])
          `shouldBe` Just (Call "foo" [ASTInteger 1, ASTInteger 5])

    describe "error cases" $ do
      it "should return Nothing for arithmetic with wrong number of args" $ do
        evalAST (Call "+" [ASTInteger 1]) `shouldBe` Nothing
        evalAST (Call "+" [ASTInteger 1, ASTInteger 2, ASTInteger 3]) `shouldBe` Nothing

      it "should return Nothing for arithmetic with non-integer args" $ do
        evalAST (Call "+" [ASTSymbol "x", ASTInteger 1]) `shouldBe` Nothing
        evalAST (Call "*" [ASTBoolean True, ASTInteger 2]) `shouldBe` Nothing

-- | Exported combined spec for Bootstrap module
bootstrapModuleSpec :: Spec
bootstrapModuleSpec = do
  describe "Bootstrap module" $ do
    sexprSpec
    printTreeSpec
    astDataSpec
    sexprToASTSpec
    evalASTSpec
