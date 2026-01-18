{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Types.AST
  ( Program (..),
    programDecls,
    Visibility (..),
    Decl (..),
    FunctionDecl (..),
    StructDecl (..),
    ModulePath (..),
    ImportTarget (..),
    ImportDecl (..),
    ErrorDecl (..),
    ErrorSetDecl (..),
    Stmt (..),
    Block (..),
    ForInit (..),
    Expr (..),
    LValue (..),
    LocatedExpr,
    LocatedStmt,
    LocatedDecl,
    LocatedProgram,
    exprSpan,
    stmtSpan,
    declSpan,
  )
where

import AST.Types.Common
  ( ErrorName,
    FieldName,
    FuncName,
    Located (..),
    ModuleName,
    SourceSpan,
    TypeName,
    VarName,
  )
import AST.Types.Literal
  ( Literal,
  )
import AST.Types.Operator
  ( AssignOp,
    BinaryOp,
    UnaryOp,
  )
import AST.Types.Type
  ( ErrorField,
    ErrorSetMember,
    Parameter,
    QualifiedType,
    StructField,
    Type,
  )
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype Program ann = Program
  { unProgram :: [Located (Decl ann)]
  }
  deriving stock (Show, Eq, Generic)

programDecls :: Program ann -> [Located (Decl ann)]
programDecls = unProgram

data Visibility
  = Public
  | Static
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Visibility

data Decl ann
  = DeclFunction Visibility (FunctionDecl ann)
  | DeclStruct Visibility StructDecl
  | DeclImport ImportDecl
  | DeclError Visibility ErrorDecl
  | DeclErrorSet Visibility ErrorSetDecl
  deriving stock (Show, Eq, Generic)

data FunctionDecl ann = FunctionDecl
  { funcDeclName :: Located FuncName,
    funcDeclParams :: [Located Parameter],
    funcDeclReturnType :: Located QualifiedType,
    funcDeclBody :: Block ann
  }
  deriving stock (Show, Eq, Generic)

data StructDecl = StructDecl
  { structDeclName :: Located TypeName,
    structDeclFields :: [Located StructField]
  }
  deriving stock (Show, Eq, Generic)

newtype ModulePath = ModulePath
  { modulePathParts :: [Located ModuleName]
  }
  deriving stock (Show, Eq, Generic)

data ImportTarget
  = -- | Import the module itself: @import math@
    ImportAll
  | -- | Import specific names: @from math import sin, cos@
    ImportNames [Located VarName]
  | -- | Import everything: @from math import *@
    ImportWildcard
  deriving stock (Show, Eq, Generic)

data ImportDecl = ImportDecl
  { importPath :: ModulePath,
    importTarget :: ImportTarget
  }
  deriving stock (Show, Eq, Generic)

data ErrorDecl = ErrorDecl
  { errorDeclName :: Located ErrorName,
    -- | Optional fields
    errorDeclFields :: [Located ErrorField]
  }
  deriving stock (Show, Eq, Generic)

data ErrorSetDecl = ErrorSetDecl
  { errorSetDeclName :: Located ErrorName,
    errorSetDeclMembers :: [Located ErrorSetMember]
  }
  deriving stock (Show, Eq, Generic)

data Block ann = Block
  { -- | Span of entire block including braces
    blockSpan :: SourceSpan,
    -- | Statements in the block
    blockStmts :: [Located (Stmt ann)]
  }
  deriving stock (Show, Eq, Generic)

data Stmt ann
  = StmtVarDecl
      (Located VarName)
      (Located QualifiedType)
      -- | Optional initializer
      (Maybe (Located (Expr ann)))
  | StmtAssign
      (Located (LValue ann))
      AssignOp
      (Located (Expr ann))
  | StmtExpr (Located (Expr ann))
  | StmtIf
      (Located (Expr ann))
      (Block ann)
      (Maybe (Block ann))
  | StmtWhile
      (Located (Expr ann))
      (Block ann)
  | StmtFor
      (Maybe (ForInit ann))
      (Maybe (Located (Expr ann)))
      (Maybe (Located (Stmt ann)))
      (Block ann)
  | StmtReturn (Maybe (Located (Expr ann)))
  | StmtBreak
  | StmtContinue
  | StmtBlock (Block ann)
  deriving stock (Show, Eq, Generic)

data ForInit ann
  = ForInitDecl
      (Located VarName)
      (Located QualifiedType)
      (Located (Expr ann))
  | ForInitExpr (Located (Expr ann))
  deriving stock (Show, Eq, Generic)

data Expr ann
  = ExprLiteral (Literal (Located (Expr ann)))
  | ExprVar (Located VarName)
  | ExprBinary
      BinaryOp
      (Located (Expr ann))
      (Located (Expr ann))
  | ExprUnary
      UnaryOp
      (Located (Expr ann))
  | ExprCall
      (Located FuncName)
      [Located (Expr ann)]
  | ExprIndex
      (Located (Expr ann))
      (Located (Expr ann))
  | ExprField
      (Located (Expr ann))
      (Located FieldName)
  | ExprStructInit
      (Located TypeName)
      [(Located FieldName, Located (Expr ann))]
  | ExprArrayInit
      (Located Type)
      [Located (Expr ann)]
  | ExprTry (Located (Expr ann))
  | ExprMust (Located (Expr ann))
  | -- Parenthesized expression (for preserving source structure if needed)
    ExprParen (Located (Expr ann))
  | -- Type cast (explicit)
    ExprCast
      (Located (Expr ann))
      (Located Type)
  deriving stock (Show, Eq, Generic)

data LValue ann
  = LVarRef (Located VarName)
  | LArrayIndex (Located (LValue ann)) (Located (Expr ann))
  | LFieldAccess (Located (LValue ann)) (Located FieldName)
  deriving stock (Show, Eq, Generic)

type LocatedExpr ann = Located (Expr ann)

type LocatedStmt ann = Located (Stmt ann)

type LocatedDecl ann = Located (Decl ann)

type LocatedProgram ann = Program ann

exprSpan :: Located (Expr ann) -> SourceSpan
exprSpan = locSpan

stmtSpan :: Located (Stmt ann) -> SourceSpan
stmtSpan = locSpan

declSpan :: Located (Decl ann) -> SourceSpan
declSpan = locSpan
