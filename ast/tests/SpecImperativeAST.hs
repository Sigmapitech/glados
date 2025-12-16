module SpecImperativeAST where

import ImperativeAST
import Test.Hspec

spec :: Spec
spec = do
  describe "ImperativeAST" $ do
    describe "Type System" $ do
      it "can represent basic types" $ do
        show intType `shouldBe` "int"
        show boolType `shouldBe` "bool"
        show (TInt Bits8 Unsigned) `shouldBe` "int`8u"
        show (TArray intType) `shouldBe` "[int]"

      it "can represent generic types" $ do
        show (genericType "T") `shouldBe` "T"
        show (funcType [genericType "T"] (genericType "T")) `shouldBe` "fn (T) -> T"

    describe "Expressions" $ do
      it "can represent literals" $ do
        show (intLit 42) `shouldBe` "42"
        show (boolLit True) `shouldBe` "True"
        show (boolLit False) `shouldBe` "False"

      it "can represent binary operations" $ do
        show (binOp Add (var "x") (intLit 2)) `shouldBe` "(x + 2)"
        show (binOp Lt (var "n") (intLit 1)) `shouldBe` "(n < 1)"

      it "can represent function calls" $ do
        show (funcCall (var "f") [var "value"]) `shouldBe` "f(value)"

      it "can represent member access" $ do
        show (MemberAccess (var "math") "sqrt") `shouldBe` "math.sqrt"

      it "can represent type casts" $ do
        show (TypeCast intType (funcCall (MemberAccess (var "math") "sqrt") [var "n"]))
          `shouldBe` "int(math.sqrt(n))"

    describe "Statements" $ do
      it "can represent variable declarations" $ do
        show (varDecl "x" (Just intType) Nothing) `shouldBe` "x: int"
        show (varDecl "n" (Just intType) (Just (intLit 3))) `shouldBe` "n: int = 3"

      it "can represent assignments" $ do
        show (assign "value" (funcCall (var "f") [var "value"]))
          `shouldBe` "value = f(value)"

      it "can represent return statements" $ do
        show (returnStmt (Just (intLit (-1)))) `shouldBe` "return -1"
        show (returnStmt Nothing) `shouldBe` "return"

      it "can represent if statements" $ do
        let ifSt =
              ifStmt
                (binOp Lt (var "k") (intLit 1))
                [returnStmt (Just (intLit (-1)))]
                Nothing
        show ifSt `shouldContain` "if"
        show ifSt `shouldContain` "(k < 1)"

      it "can represent while loops" $ do
        let wl = whileLoop (binOp Gt (var "k") (intLit 0)) [assign "k" (binOp Sub (var "k") (intLit 1))]
        show wl `shouldContain` "while"
        show wl `shouldContain` "(k > 0)"

      it "can represent for loops" $ do
        let forL =
              forLoop
                (Just (varDecl "i" (Just intType) (Just (intLit 1))))
                (Just (binOp Lt (var "i") (intLit 21)))
                (Just (UnaryOp Neg (var "i"))) -- simplified increment
                [ExprStmt (funcCall (var "print") [funcCall (var "nth_prime") [var "i"]])]
        show forL `shouldContain` "for"

      it "can represent for-each loops" $ do
        let forEach =
              forEachLoop
                "f"
                Nothing
                (var "applicators")
                [assign "value" (funcCall (var "f") [var "value"])]
        show forEach `shouldContain` "for"
        show forEach `shouldContain` "applicators"

    describe "Function Declarations" $ do
      it "can represent simple functions" $ do
        let func =
              funcDecl
                "add"
                []
                [param "x" intType, param "y" intType]
                intType
                [returnStmt (Just (binOp Add (var "x") (var "y")))]
        show func `shouldContain` "func add"
        show func `shouldContain` "x: int"
        show func `shouldContain` "-> int"

      it "can represent generic functions" $ do
        let func =
              funcDecl
                "identity"
                ["T"]
                [param "x" (genericType "T")]
                (genericType "T")
                [returnStmt (Just (var "x"))]
        show func `shouldContain` "func<T>"
        show func `shouldContain` "identity"

    describe "Complete Program Examples" $ do
      it "can represent the pipe_reduce function" $ do
        let pipeReduceFunc =
              funcDecl
                "pipe_reduce"
                ["T"]
                [ param "applicators" (arrayType (funcType [genericType "T"] (genericType "T"))),
                  param "value" (genericType "T")
                ]
                (genericType "T")
                [ forEachLoop
                    "f"
                    Nothing
                    (var "applicators")
                    [assign "value" (funcCall (var "applicator") [var "value"])],
                  returnStmt (Just (var "value"))
                ]

        show pipeReduceFunc `shouldContain` "func<T> pipe_reduce"
        show pipeReduceFunc `shouldContain` "applicators: [fn (T) -> T]"
        show pipeReduceFunc `shouldContain` "value: T"
        show pipeReduceFunc `shouldContain` "-> T"

      it "can represent the nth_prime function" $ do
        let nthPrimeFunc =
              funcDecl
                "nth_prime"
                []
                [param "k" (TInt Bits8 Unsigned)]
                intType
                [ ifStmt
                    (binOp Lt (var "k") (intLit 1))
                    [returnStmt (Just (intLit (-1)))]
                    Nothing,
                  ifStmt
                    (binOp Eq (var "k") (intLit 1))
                    [returnStmt (Just (intLit 2))]
                    Nothing,
                  ifStmt
                    (binOp Eq (var "k") (intLit 2))
                    [returnStmt (Just (intLit 3))]
                    Nothing,
                  varDecl "n" (Just intType) (Just (intLit 3)),
                  assign "k" (binOp Sub (var "k") (intLit 2)),
                  whileLoop
                    (binOp Gt (var "k") (intLit 0))
                    [ assign "n" (binOp Add (var "n") (intLit 2)),
                      assign "is_prime" (boolLit True),
                      forLoop
                        (Just (varDecl "i" (Just intType) (Just (intLit 3))))
                        ( Just
                            ( binOp
                                Lt
                                (var "i")
                                ( binOp
                                    Add
                                    ( TypeCast
                                        intType
                                        (funcCall (MemberAccess (var "math") "sqrt") [var "n"])
                                    )
                                    (intLit 1)
                                )
                            )
                        )
                        (Just (UnaryOp Neg (var "i")))
                        [ ifStmt
                            (binOp Eq (binOp Mod (var "n") (var "i")) (intLit 0))
                            [assign "is_prime" (boolLit False)]
                            Nothing
                        ],
                      ifStmt
                        (var "is_prime")
                        [assign "k" (binOp Sub (var "k") (intLit 1))]
                        Nothing
                    ],
                  returnStmt (Just (var "n"))
                ]

        show nthPrimeFunc `shouldContain` "func nth_prime"
        show nthPrimeFunc `shouldContain` "k: int`8u"
        show nthPrimeFunc `shouldContain` "-> int"
        show nthPrimeFunc `shouldContain` "n: int = 3"

      it "can represent the main function" $ do
        let mainFunc =
              funcDecl
                "main"
                []
                []
                intType
                [ forLoop
                    (Just (varDecl "i" (Just intType) (Just (intLit 1))))
                    (Just (binOp Lt (var "i") (intLit 21)))
                    (Just (UnaryOp Neg (var "i")))
                    [ ExprStmt
                        ( funcCall
                            (var "print")
                            [funcCall (var "nth_prime") [var "i"]]
                        )
                    ]
                ]

        show mainFunc `shouldContain` "func main"
        show mainFunc `shouldContain` "-> int"

      it "can represent a complete program" $ do
        let program =
              Program
                [ importDecl "math",
                  funcDecl
                    "simple"
                    []
                    [param "x" intType]
                    intType
                    [returnStmt (Just (binOp Add (var "x") (intLit 1)))]
                ]

        show program `shouldContain` "import math"
        show program `shouldContain` "func simple"

    describe "Helper Functions" $ do
      it "provides convenient constructors" $ do
        param "x" intType `shouldBe` Parameter (VarName "x") intType
        unVarName (VarName "test") `shouldBe` "test"
        unFuncName (FuncName "foo") `shouldBe` "foo"
        unTypeVarName (TypeVarName "T") `shouldBe` "T"
        unModuleName (ModuleName "math") `shouldBe` "math"
