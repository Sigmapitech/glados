module LispAstSpec (astModuleSpec) where

import Ast hiding (initialEnv)
import Data.Void (Void)
import Test.Hspec

-- | Test suite for newtype wrappers and their extractors
newtypeSpec :: Spec
newtypeSpec = do
  describe "VarName" $ do
    it "should create and extract VarName" $ do
      unVarName (VarName "x") `shouldBe` "x"
      unVarName (VarName "") `shouldBe` ""
      unVarName (VarName "myVariable") `shouldBe` "myVariable"

    it "should support IsString for VarName" $ do
      unVarName "test" `shouldBe` "test"

    it "should compare VarNames for equality" $ do
      VarName "x" `shouldBe` VarName "x"
      VarName "x" `shouldNotBe` VarName "y"

  describe "ParamName" $ do
    it "should create and extract ParamName" $ do
      unParamName (ParamName "param1") `shouldBe` "param1"
      unParamName (ParamName "") `shouldBe` ""

    it "should support IsString for ParamName" $ do
      unParamName "param" `shouldBe` "param"

    it "should compare ParamNames for equality" $ do
      ParamName "a" `shouldBe` ParamName "a"
      ParamName "a" `shouldNotBe` ParamName "b"

  describe "SymbolName" $ do
    it "should create and extract SymbolName" $ do
      unSymbolName (SymbolName "define") `shouldBe` "define"
      unSymbolName (SymbolName "+") `shouldBe` "+"

    it "should support IsString for SymbolName" $ do
      unSymbolName "lambda" `shouldBe` "lambda"

    it "should compare SymbolNames for equality" $ do
      SymbolName "if" `shouldBe` SymbolName "if"
      SymbolName "if" `shouldNotBe` SymbolName "else"

  describe "ErrorMsg" $ do
    it "should create and extract ErrorMsg" $ do
      unErrorMsg (ErrorMsg "error") `shouldBe` "error"
      unErrorMsg (ErrorMsg "") `shouldBe` ""

    it "should support IsString for ErrorMsg" $ do
      unErrorMsg "some error" `shouldBe` "some error"

    it "should compare ErrorMsgs for equality" $ do
      ErrorMsg "err1" `shouldBe` ErrorMsg "err1"
      ErrorMsg "err1" `shouldNotBe` ErrorMsg "err2"

-- | Test suite for name conversion functions
conversionSpec :: Spec
conversionSpec = do
  describe "paramToVar" $ do
    it "should convert ParamName to VarName" $ do
      paramToVar (ParamName "x") `shouldBe` VarName "x"
      paramToVar (ParamName "myParam") `shouldBe` VarName "myParam"

  describe "varToParam" $ do
    it "should convert VarName to ParamName" $ do
      varToParam (VarName "x") `shouldBe` ParamName "x"
      varToParam (VarName "myVar") `shouldBe` ParamName "myVar"

  describe "symbolToVar" $ do
    it "should convert SymbolName to VarName" $ do
      symbolToVar (SymbolName "func") `shouldBe` VarName "func"
      symbolToVar (SymbolName "+") `shouldBe` VarName "+"

  describe "roundtrip conversions" $ do
    it "should preserve value through paramToVar . varToParam" $ do
      let var = VarName "test"
      paramToVar (varToParam var) `shouldBe` var

    it "should preserve value through varToParam . paramToVar" $ do
      let param = ParamName "test"
      varToParam (paramToVar param) `shouldBe` param

-- | Test suite for SExpr data type
sexprSpec :: Spec
sexprSpec = do
  describe "SExpr" $ do
    it "should create SInteger" $ do
      SInteger 42 `shouldBe` SInteger 42
      SInteger (-1) `shouldBe` SInteger (-1)
      SInteger 0 `shouldBe` SInteger 0

    it "should create SSymbol" $ do
      SSymbol (SymbolName "x") `shouldBe` SSymbol "x"
      SSymbol (SymbolName "+") `shouldBe` SSymbol "+"

    it "should create SBool" $ do
      SBool True `shouldBe` SBool True
      SBool False `shouldBe` SBool False

    it "should create SList" $ do
      SList [] `shouldBe` SList []
      SList [SInteger 1, SInteger 2] `shouldBe` SList [SInteger 1, SInteger 2]

    it "should create nested SList" $ do
      let nested = SList [SSymbol "+", SList [SInteger 1, SInteger 2]]
      nested `shouldBe` SList [SSymbol (SymbolName "+"), SList [SInteger 1, SInteger 2]]

    it "should distinguish different SExpr constructors" $ do
      SInteger 1 `shouldNotBe` SBool True
      SSymbol "x" `shouldNotBe` SList []

-- | Test suite for Ast data type
astSpec :: Spec
astSpec = do
  describe "Ast" $ do
    it "should create LiteralInt" $ do
      LiteralInt 42 `shouldBe` LiteralInt 42
      LiteralInt (-100) `shouldBe` LiteralInt (-100)

    it "should create LiteralBool" $ do
      LiteralBool True `shouldBe` LiteralBool True
      LiteralBool False `shouldBe` LiteralBool False

    it "should create VariableRef" $ do
      VariableRef (VarName "x") `shouldBe` VariableRef (VarName "x")
      VariableRef "y" `shouldBe` VariableRef (VarName "y")

    it "should create Define" $ do
      Define (VarName "x") (LiteralInt 5) `shouldBe` Define "x" (LiteralInt 5)

    it "should create Define for functions" $ do
      let funcDef = Define (VarName "add") (Lambda [ParamName "a", ParamName "b"] (Call (VariableRef "+") [VariableRef "a", VariableRef "b"]))
      funcDef `shouldBe` Define "add" (Lambda ["a", "b"] (Call (VariableRef "+") [VariableRef "a", VariableRef "b"]))

    it "should create Lambda" $ do
      let lambda = Lambda [ParamName "x", ParamName "y"] (VariableRef "x")
      lambda `shouldBe` Lambda ["x", "y"] (VariableRef "x")

    it "should create Call" $ do
      let call = Call (VariableRef "+") [LiteralInt 1, LiteralInt 2]
      call `shouldBe` Call (VariableRef "+") [LiteralInt 1, LiteralInt 2]

    it "should create If" $ do
      let ifExpr = If (LiteralBool True) (LiteralInt 1) (LiteralInt 0)
      ifCond ifExpr `shouldBe` LiteralBool True
      ifThen ifExpr `shouldBe` LiteralInt 1
      ifElse ifExpr `shouldBe` LiteralInt 0

    it "should create nested Ast structures" $ do
      let nested =
            Define
              "factorial"
              ( Lambda
                  ["n"]
                  ( If
                      (Call (VariableRef "eq?") [VariableRef "n", LiteralInt 0])
                      (LiteralInt 1)
                      (Call (VariableRef "*") [VariableRef "n", Call (VariableRef "factorial") [Call (VariableRef "-") [VariableRef "n", LiteralInt 1]]])
                  )
              )
      nested `shouldBe` nested

-- | Test suite for RuntimeValue data type
runtimeValueSpec :: Spec
runtimeValueSpec = do
  describe "RuntimeValue" $ do
    it "should compare VInt values" $ do
      VInt 42 `shouldBe` VInt 42
      VInt (-1) `shouldBe` VInt (-1)
      VInt 0 `shouldNotBe` VInt 1

    it "should compare VBool values" $ do
      VBool True `shouldBe` VBool True
      VBool False `shouldBe` VBool False
      VBool True `shouldNotBe` VBool False

    it "should compare VUnit values" $ do
      VUnit `shouldBe` VUnit
      VUnit == VUnit `shouldNotBe` False

    it "should compare VProcedure values" $ do
      let f = VProcedure Nothing ["x"] (Call (VariableRef "+") [VariableRef "x", LiteralInt 1])
      f `shouldBe` f

    it "should not compare different VProcedure values as equal" $ do
      let f1 = VProcedure Nothing ["x"] (Call (VariableRef "+") [VariableRef "x", LiteralInt 1])
      let f2 = VProcedure Nothing ["y"] (Call (VariableRef "+") [VariableRef "y", LiteralInt 2])
      f1 `shouldNotBe` f2

    it "should compare VBuiltin values" $ do
      VBuiltin BPlus `shouldBe` VBuiltin BPlus
      VBuiltin BPlus `shouldNotBe` VBuiltin BMinus

    it "should not compare different RuntimeValue constructors as equal" $ do
      VInt 1 `shouldNotBe` VBool True
      VBool True `shouldNotBe` VUnit
      VInt 0 `shouldNotBe` VUnit

-- | Test suite for Environment functions
environmentSpec :: Spec
environmentSpec = do
  describe "emptyEnv" $ do
    it "should create an empty environment" $ do
      emptyEnv `shouldBe` []
      length emptyEnv `shouldBe` 0

  describe "extendEnv" $ do
    it "should add a binding to an empty environment" $ do
      let env = extendEnv "x" (VInt 5) emptyEnv
      env `shouldBe` [("x", VInt 5)]

    it "should add multiple bindings" $ do
      let env =
            extendEnv "y" (VBool True) $
              extendEnv "x" (VInt 5) emptyEnv
      length env `shouldBe` 2
      env `shouldBe` [("y", VBool True), ("x", VInt 5)]

    it "should shadow previous bindings with same name" $ do
      let env =
            extendEnv "x" (VInt 10) $
              extendEnv "x" (VInt 5) emptyEnv
      env `shouldBe` [("x", VInt 10), ("x", VInt 5)]
      lookupEnv "x" env `shouldBe` Just (VInt 10)

  describe "lookupEnv" $ do
    it "should return Nothing for empty environment" $ do
      lookupEnv "x" emptyEnv `shouldBe` Nothing

    it "should find existing binding" $ do
      let env = extendEnv "x" (VInt 42) emptyEnv
      lookupEnv "x" env `shouldBe` Just (VInt 42)

    it "should return Nothing for non-existing binding" $ do
      let env = extendEnv "x" (VInt 42) emptyEnv
      lookupEnv "y" env `shouldBe` Nothing

    it "should find the most recent binding when shadowed" $ do
      let env =
            extendEnv "x" (VInt 20) $
              extendEnv "x" (VInt 10) emptyEnv
      lookupEnv "x" env `shouldBe` Just (VInt 20)

    it "should find bindings in a larger environment" $ do
      let env =
            extendEnv "c" (VBool False) $
              extendEnv "b" (VBool True) $
                extendEnv "a" (VInt 1) emptyEnv
      lookupEnv "a" env `shouldBe` Just (VInt 1)
      lookupEnv "b" env `shouldBe` Just (VBool True)
      lookupEnv "c" env `shouldBe` Just (VBool False)

-- | Test suite for Evaluator monad
evaluatorSpec :: Spec
evaluatorSpec = do
  describe "runEvaluator" $ do
    it "should run a successful computation" $ do
      let computation = return (VInt 42)
      let (result, _) = runEvaluator computation emptyEnv
      result `shouldBe` Right (VInt 42)

    it "should preserve the initial environment" $ do
      let computation = return VUnit
      let initialEnv = extendEnv "x" (VInt 5) emptyEnv
      let (_, finalEnv) = runEvaluator computation initialEnv
      finalEnv `shouldBe` initialEnv

  describe "throwEvalError" $ do
    it "should create an error result" $ do
      let computation = throwEvalError "test error"
      let (result, _) = runEvaluator computation emptyEnv
      result `shouldBe` Left (ErrorMsg "test error")

-- | Test suite for error handling helpers
errorSpec :: Spec
errorSpec = do
  describe "mkError" $ do
    it "should create an ErrorMsg" $ do
      mkError "test" `shouldBe` ErrorMsg "test"
      mkError "" `shouldBe` ErrorMsg ""

  describe "liftError" $ do
    it "should lift Right values unchanged" $ do
      liftError "context" (Right (42 :: Int)) `shouldBe` Right 42

    it "should add context to Left values" $ do
      liftError "parsing" (Left "syntax error" :: Either String Void) `shouldBe` Left (ErrorMsg "parsing: syntax error")

  describe "addErrContext" $ do
    it "should add context to Left values" $ do
      addErrContext "evaluation" (Left (ErrorMsg "division by zero") :: Result Void) `shouldBe` Left (ErrorMsg "evaluation: division by zero")

    it "should leave Right values unchanged" $ do
      addErrContext "context" (Right (VInt 10)) `shouldBe` Right (VInt 10)

-- | Test suite for built-in operators
builtinSpec :: Spec
builtinSpec = do
  describe "arithmetic operators" $ do
    it "should define builtinPlus" $ do
      builtinPlus `shouldBe` VarName "+"

    it "should define builtinMinus" $ do
      builtinMinus `shouldBe` VarName "-"

    it "should define builtinMult" $ do
      builtinMult `shouldBe` VarName "*"

    it "should define builtinDiv" $ do
      builtinDiv `shouldBe` VarName "div"

    it "should define builtinMod" $ do
      builtinMod `shouldBe` VarName "mod"

  describe "comparison operators" $ do
    it "should define builtinEq" $ do
      builtinEq `shouldBe` VarName "eq?"

    it "should define builtinLt" $ do
      builtinLt `shouldBe` VarName "<"

  describe "isBuiltin" $ do
    it "should recognize arithmetic operators" $ do
      isBuiltin builtinPlus `shouldBe` True
      isBuiltin builtinMinus `shouldBe` True
      isBuiltin builtinMult `shouldBe` True
      isBuiltin builtinDiv `shouldBe` True
      isBuiltin builtinMod `shouldBe` True

    it "should recognize comparison operators" $ do
      isBuiltin builtinEq `shouldBe` True
      isBuiltin builtinLt `shouldBe` True

    it "should not recognize non-builtin names" $ do
      isBuiltin (VarName "foo") `shouldBe` False
      isBuiltin (VarName "bar") `shouldBe` False
      isBuiltin (VarName "define") `shouldBe` False

-- | Test suite for special form symbols
symbolSpec :: Spec
symbolSpec = do
  describe "special form symbols" $ do
    it "should define defineSymbol" $ do
      defineSymbol `shouldBe` SymbolName "define"

    it "should define lambdaSymbol" $ do
      lambdaSymbol `shouldBe` SymbolName "lambda"

    it "should define ifSymbol" $ do
      ifSymbol `shouldBe` SymbolName "if"

-- | Exported combined spec for Ast module
astModuleSpec :: Spec
astModuleSpec = do
  describe "Ast module" $ do
    newtypeSpec
    conversionSpec
    sexprSpec
    astSpec
    runtimeValueSpec
    environmentSpec
    evaluatorSpec
    errorSpec
    builtinSpec
    symbolSpec
