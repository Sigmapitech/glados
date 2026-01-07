module SpecLispSexprtoAST (specSexprtoAST) where

import AST.Lisp.AST
import AST.Lisp.SexprtoAST
import Test.Hspec

specSexprtoAST :: Spec
specSexprtoAST = do
  describe "sexprToAST" $ do
    it "converts integer literal" $ do
      sexprToAST (SInteger 42) `shouldBe` Right (LiteralInt 42)

    it "converts boolean literal" $ do
      sexprToAST (SBool True) `shouldBe` Right (LiteralBool True)
      sexprToAST (SBool False) `shouldBe` Right (LiteralBool False)

    it "converts symbol to VariableRef" $ do
      sexprToAST (SSymbol (SymbolName "x")) `shouldBe` Right (VariableRef (VarName "x"))

    it "converts define form" $ do
      let expr = SList [SSymbol defineSymbol, SSymbol (SymbolName "x"), SInteger 1]
      sexprToAST expr `shouldBe` Right (Define (VarName "x") (LiteralInt 1))

    it "errors on malformed define" $ do
      let expr = SList [SSymbol defineSymbol, SInteger 1]
      sexprToAST expr `shouldSatisfy` isLeft

    it "converts lambda form" $ do
      let expr = SList [SSymbol lambdaSymbol, SList [SSymbol (SymbolName "a"), SSymbol (SymbolName "b")], SSymbol (SymbolName "a")]
      sexprToAST expr `shouldBe` Right (Lambda [ParamName "a", ParamName "b"] (VariableRef (VarName "a")))

    it "errors on malformed lambda" $ do
      let expr = SList [SSymbol lambdaSymbol, SSymbol (SymbolName "a"), SSymbol (SymbolName "a")]
      sexprToAST expr `shouldSatisfy` isLeft

    it "converts if form" $ do
      let expr = SList [SSymbol ifSymbol, SBool True, SInteger 1, SInteger 2]
      sexprToAST expr `shouldBe` Right (If (LiteralBool True) (LiteralInt 1) (LiteralInt 2))

    it "errors on malformed if" $ do
      let expr = SList [SSymbol ifSymbol, SBool True, SInteger 1]
      sexprToAST expr `shouldSatisfy` isLeft

    it "converts function call" $ do
      let expr = SList [SSymbol (SymbolName "+"), SInteger 1, SInteger 2]
      sexprToAST expr `shouldBe` Right (Call (VariableRef (VarName "+")) [LiteralInt 1, LiteralInt 2])

    it "errors on empty list" $ do
      sexprToAST (SList []) `shouldSatisfy` isLeft

    it "converts nested function call" $ do
      let expr = SList [SSymbol (SymbolName "+"), SInteger 1, SList [SSymbol (SymbolName "*"), SInteger 2, SInteger 3]]
      sexprToAST expr `shouldBe` Right (Call (VariableRef (VarName "+")) [LiteralInt 1, Call (VariableRef (VarName "*")) [LiteralInt 2, LiteralInt 3]])

    it "converts lambda with call in body" $ do
      let expr = SList [SSymbol lambdaSymbol, SList [SSymbol (SymbolName "x")], SList [SSymbol (SymbolName "-"), SSymbol (SymbolName "x"), SInteger 1]]
      sexprToAST expr `shouldBe` Right (Lambda [ParamName "x"] (Call (VariableRef (VarName "-")) [VariableRef (VarName "x"), LiteralInt 1]))

    it "converts if with nested calls" $ do
      let expr = SList [SSymbol ifSymbol, SList [SSymbol (SymbolName "eq?"), SInteger 1, SInteger 2], SInteger 42, SList [SSymbol (SymbolName "+"), SInteger 1, SInteger 2]]
      sexprToAST expr `shouldBe` Right (If (Call (VariableRef (VarName "eq?")) [LiteralInt 1, LiteralInt 2]) (LiteralInt 42) (Call (VariableRef (VarName "+")) [LiteralInt 1, LiteralInt 2]))

    it "converts define with lambda as value" $ do
      let expr = SList [SSymbol defineSymbol, SSymbol (SymbolName "f"), SList [SSymbol lambdaSymbol, SList [SSymbol (SymbolName "x")], SList [SSymbol (SymbolName "*"), SSymbol (SymbolName "x"), SInteger 2]]]
      sexprToAST expr `shouldBe` Right (Define (VarName "f") (Lambda [ParamName "x"] (Call (VariableRef (VarName "*")) [VariableRef (VarName "x"), LiteralInt 2])))

    it "converts deeply nested expressions" $ do
      let expr = SList [SSymbol defineSymbol, SSymbol (SymbolName "result"), SList [SSymbol ifSymbol, SList [SSymbol (SymbolName "<"), SInteger 1, SInteger 2], SList [SSymbol (SymbolName "+"), SInteger 1, SInteger 2], SList [SSymbol (SymbolName "-"), SInteger 1, SInteger 2]]]
      sexprToAST expr `shouldBe` Right (Define (VarName "result") (If (Call (VariableRef (VarName "<")) [LiteralInt 1, LiteralInt 2]) (Call (VariableRef (VarName "+")) [LiteralInt 1, LiteralInt 2]) (Call (VariableRef (VarName "-")) [LiteralInt 1, LiteralInt 2])))

-- Helper for error checking
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
