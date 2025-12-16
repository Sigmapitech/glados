{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Abstract Syntax Tree definitions for the imperative language.
-- This module defines the core data types for representing parsed programs,
-- AST nodes, types, expressions, statements, and declarations.
module ImperativeAST where

import Data.List (intercalate)
import Data.String (IsString (..))

newtype VarName = VarName String
  deriving (Show, Eq, Ord, IsString)

newtype FuncName = FuncName String
  deriving (Show, Eq, Ord, IsString)

newtype TypeVarName = TypeVarName String
  deriving (Show, Eq, Ord, IsString)

newtype ModuleName = ModuleName String
  deriving (Show, Eq, Ord, IsString)

unVarName :: VarName -> String
unVarName (VarName s) = s

unFuncName :: FuncName -> String
unFuncName (FuncName s) = s

unTypeVarName :: TypeVarName -> String
unTypeVarName (TypeVarName s) = s

unModuleName :: ModuleName -> String
unModuleName (ModuleName s) = s

data Type
  = TInt IntSize Signedness
  | TBool
  | TArray Type
  | TFunction [Type] Type
  | TGeneric TypeVarName
  | TUnit
  deriving (Eq, Ord)

data IntSize
  = DefaultSize
  | Bits8
  | Bits16
  | Bits32
  | Bits64
  deriving (Eq, Ord)

data Signedness
  = Signed
  | Unsigned
  deriving (Eq, Ord)

instance Show Type where
  show (TInt size sign) = "int" ++ showSize size ++ showSign sign
    where
      showSize DefaultSize = ""
      showSize Bits8 = "`8"
      showSize Bits16 = "`16"
      showSize Bits32 = "`32"
      showSize Bits64 = "`64"
      showSign Signed = ""
      showSign Unsigned = "u"
  show TBool = "bool"
  show (TArray t) = "[" ++ show t ++ "]"
  show (TFunction params ret) =
    "fn (" ++ intercalate ", " (map show params) ++ ") -> " ++ show ret
  show (TGeneric name) = unTypeVarName name
  show TUnit = "void"

data Expr
  = IntLit Integer
  | BoolLit Bool
  | Var VarName
  | BinOp BinaryOp Expr Expr
  | UnaryOp UnaryOp Expr
  | FuncCall Expr [Expr]
  | ArrayIndex Expr Expr
  | MemberAccess Expr VarName
  | TypeCast Type Expr
  deriving (Eq)

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor
  | ShiftLeft
  | ShiftRight
  deriving (Eq, Ord)

data UnaryOp
  = Neg
  | Not
  | BitNot
  deriving (Eq, Ord)

instance Show BinaryOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Eq = "=="
  show Neq = "!="
  show Lt = "<"
  show Gt = ">"
  show Lte = "<="
  show Gte = ">="
  show And = "&&"
  show Or = "||"
  show BitAnd = "&"
  show BitOr = "|"
  show BitXor = "^"
  show ShiftLeft = "<<"
  show ShiftRight = ">>"

instance Show UnaryOp where
  show Neg = "-"
  show Not = "!"
  show BitNot = "~"

instance Show Expr where
  show (IntLit n) = show n
  show (BoolLit True) = "True"
  show (BoolLit False) = "False"
  show (Var name) = unVarName name
  show (BinOp op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (UnaryOp op e) = show op ++ show e
  show (FuncCall f args) = show f ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ArrayIndex arr idx) = show arr ++ "[" ++ show idx ++ "]"
  show (MemberAccess obj member) = show obj ++ "." ++ unVarName member
  show (TypeCast t e) = show t ++ "(" ++ show e ++ ")"

data Statement
  = VarDecl VarName (Maybe Type) (Maybe Expr)
  | Assign LValue Expr
  | ExprStmt Expr
  | Return (Maybe Expr)
  | IfStmt Expr [Statement] (Maybe [Statement])
  | WhileLoop Expr [Statement]
  | ForLoop (Maybe Statement) (Maybe Expr) (Maybe Expr) [Statement]
  | ForEachLoop VarName (Maybe Type) Expr [Statement]
  | Block [Statement]
  | Break
  | Continue
  deriving (Eq)

data LValue
  = LVar VarName
  | LArrayIndex LValue Expr
  | LMemberAccess LValue VarName
  deriving (Eq)

instance Show LValue where
  show (LVar name) = unVarName name
  show (LArrayIndex lv idx) = show lv ++ "[" ++ show idx ++ "]"
  show (LMemberAccess lv member) = show lv ++ "." ++ unVarName member

instance Show Statement where
  show (VarDecl name mType mInit) =
    unVarName name
      ++ maybe "" (\t -> ": " ++ show t) mType
      ++ maybe "" (\e -> " = " ++ show e) mInit
  show (Assign lv e) = show lv ++ " = " ++ show e
  show (ExprStmt e) = show e
  show (Return Nothing) = "return"
  show (Return (Just e)) = "return " ++ show e
  show (IfStmt cond thenStmts mElse) =
    "if ("
      ++ show cond
      ++ ") { "
      ++ intercalate "; " (map show thenStmts)
      ++ " }"
      ++ maybe "" (\els -> " else { " ++ intercalate "; " (map show els) ++ " }") mElse
  show (WhileLoop cond body) =
    "while (" ++ show cond ++ ") { " ++ intercalate "; " (map show body) ++ " }"
  show (ForLoop mInit mCond mPost body) =
    "for ("
      ++ maybe "" show mInit
      ++ "; "
      ++ maybe "" show mCond
      ++ "; "
      ++ maybe "" show mPost
      ++ ") { "
      ++ intercalate "; " (map show body)
      ++ " }"
  show (ForEachLoop loopVar mType iter body) =
    "for ("
      ++ unVarName loopVar
      ++ maybe "" (\t -> ": " ++ show t) mType
      ++ ", "
      ++ show iter
      ++ ") { "
      ++ intercalate "; " (map show body)
      ++ " }"
  show (Block stmts) = "{ " ++ intercalate "; " (map show stmts) ++ " }"
  show Break = "break"
  show Continue = "continue"

data Parameter = Parameter
  { paramName :: VarName,
    paramType :: Type
  }
  deriving (Eq)

instance Show Parameter where
  show (Parameter name ty) = unVarName name ++ ": " ++ show ty

data Declaration
  = FuncDecl
      { funcName :: FuncName,
        funcTypeParams :: [TypeVarName],
        funcParams :: [Parameter],
        funcReturnType :: Type,
        funcBody :: [Statement]
      }
  | ImportDecl ModuleName
  deriving (Eq)

instance Show Declaration where
  show (FuncDecl name typeParams params retType body) =
    "func"
      ++ (if null typeParams then "" else "<" ++ intercalate ", " (map unTypeVarName typeParams) ++ ">")
      ++ " "
      ++ unFuncName name
      ++ "("
      ++ intercalate ", " (map show params)
      ++ ") -> "
      ++ show retType
      ++ " {\n  "
      ++ intercalate "\n  " (map show body)
      ++ "\n}"
  show (ImportDecl modName) = "import " ++ unModuleName modName

newtype Program = Program [Declaration]
  deriving (Eq)

instance Show Program where
  show (Program decls) = intercalate "\n\n" (map show decls)

intType :: Type
intType = TInt DefaultSize Signed

boolType :: Type
boolType = TBool

arrayType :: Type -> Type
arrayType = TArray

funcType :: [Type] -> Type -> Type
funcType = TFunction

genericType :: String -> Type
genericType = TGeneric . TypeVarName

param :: String -> Type -> Parameter
param name = Parameter (VarName name)

funcDecl :: String -> [String] -> [Parameter] -> Type -> [Statement] -> Declaration
funcDecl name typeParams =
  FuncDecl
    (FuncName name)
    (map TypeVarName typeParams)

importDecl :: String -> Declaration
importDecl = ImportDecl . ModuleName

varDecl :: String -> Maybe Type -> Maybe Expr -> Statement
varDecl name = VarDecl (VarName name)

assign :: String -> Expr -> Statement
assign name = Assign (LVar (VarName name))

var :: String -> Expr
var = Var . VarName

intLit :: Integer -> Expr
intLit = IntLit

boolLit :: Bool -> Expr
boolLit = BoolLit

binOp :: BinaryOp -> Expr -> Expr -> Expr
binOp = BinOp

funcCall :: Expr -> [Expr] -> Expr
funcCall = FuncCall

returnStmt :: Maybe Expr -> Statement
returnStmt = Return

ifStmt :: Expr -> [Statement] -> Maybe [Statement] -> Statement
ifStmt = IfStmt

whileLoop :: Expr -> [Statement] -> Statement
whileLoop = WhileLoop

forLoop :: Maybe Statement -> Maybe Expr -> Maybe Expr -> [Statement] -> Statement
forLoop = ForLoop

forEachLoop :: String -> Maybe Type -> Expr -> [Statement] -> Statement
forEachLoop name = ForEachLoop (VarName name)
