{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Formatter
  ( formatFile,
    formatProgram,
    FormatOptions (..),
    defaultFormatOptions,
    formatOptionsCodec,
    formatImportDecl,
    formatType,
    formatQualifiedType,
    formatBinaryOp,
    formatUnaryOp,
    formatAssignOp,
    formatVisibility,
    indentation,
    precedence,
  )
where

import AST.Types
  ( ArrayType (ArrayType),
    AssignOp,
    BinaryOp,
    Block (Block),
    Constness (Const, Mutable),
    Decl (..),
    ErrorDecl (ErrorDecl),
    ErrorField (ErrorField),
    ErrorName (unErrorName),
    ErrorSetDecl (ErrorSetDecl),
    ErrorSetMember (ErrorMemberSet, ErrorMemberSingle),
    Expr (..),
    FieldName (unFieldName),
    ForInit (..),
    FuncName (unFuncName),
    FunctionDecl
      ( funcDeclBody,
        funcDeclName,
        funcDeclParams,
        funcDeclReturnType
      ),
    ImportDecl (ImportDecl),
    ImportTarget (ImportAll, ImportNames, ImportWildcard),
    LValue (..),
    Literal,
    Located (Located),
    ModuleName (unModuleName),
    ModulePath (ModulePath),
    Parameter (Parameter),
    PrimitiveType (..),
    Program (Program),
    QualifiedType (QualifiedType, qualType),
    Stmt (..),
    StructDecl (StructDecl),
    StructField (StructField),
    Type (..),
    TypeName (unTypeName),
    UnaryOp,
    VarName (unVarName),
    Visibility (..),
    assignOpSymbol,
    binaryOpPrecedence,
    binaryOpSymbol,
    unLocated,
    unaryOpSymbol,
  )
import AST.Types.Common ()
import Data.Text (Text)
import qualified Data.Text as T
import Toml (TomlCodec, (.=))
import qualified Toml

-- | Formatting options
data FormatOptions = FormatOptions
  { -- | Number of spaces per indentation level
    formatIndentSize :: Int,
    -- | Maximum line length before wrapping
    formatMaxLineLength :: Int,
    -- | Use spaces instead of tabs
    formatUseSpaces :: Bool,
    -- | Maximum width for arrays before wrapping
    formatArrayWidth :: Int,
    -- | Reorder imports alphabetically
    formatReorderImports :: Bool,
    -- | Normalize comments
    formatNormalizeComments :: Bool
  }

defaultFormatOptions :: FormatOptions
defaultFormatOptions =
  FormatOptions
    { formatIndentSize = 4,
      formatMaxLineLength = 100,
      formatUseSpaces = True,
      formatArrayWidth = 80,
      formatReorderImports = False,
      formatNormalizeComments = False
    }

-- | TOML codec for FormatOptions
formatOptionsCodec :: TomlCodec FormatOptions
formatOptionsCodec =
  FormatOptions
    <$> Toml.int "indent_size" .= formatIndentSize
    <*> Toml.int "max_width" .= formatMaxLineLength
    <*> (not <$> Toml.bool "hard_tabs") .= (not . formatUseSpaces)
    <*> Toml.int "array_width" .= formatArrayWidth
    <*> Toml.bool "reorder_imports" .= formatReorderImports
    <*> Toml.bool "normalize_comments" .= formatNormalizeComments

-- | Format a complete program
formatProgram :: FormatOptions -> Program ann -> Text
formatProgram opts (Program decls) =
  T.intercalate "\n\n" $ map (formatDecl opts 0 . unLocated) decls

-- | Format a file from source text
formatFile :: FormatOptions -> Program ann -> Text
formatFile = formatProgram

-- | Format a declaration
formatDecl :: FormatOptions -> Int -> Decl ann -> Text
formatDecl opts level = \case
  DeclFunction vis funcDecl -> formatFunctionDecl opts level vis funcDecl
  DeclImport importDecl -> formatImportDecl importDecl
  DeclStruct vis structDecl -> formatStructDecl opts level vis structDecl
  DeclError vis errorDecl -> formatErrorDecl opts level vis errorDecl
  DeclErrorSet vis errorSet -> formatErrorSetDecl opts level vis errorSet

-- | Format visibility modifier
formatVisibility :: Visibility -> Text
formatVisibility Public = "pub"
formatVisibility Static = "static"

-- | Format function declaration
formatFunctionDecl :: FormatOptions -> Int -> Visibility -> FunctionDecl ann -> Text
formatFunctionDecl opts level vis funcDecl =
  let name = unFuncName . unLocated $ funcDeclName funcDecl
      params = formatParams opts $ map unLocated $ funcDeclParams funcDecl
      retType = formatQualifiedType $ unLocated $ funcDeclReturnType funcDecl
      body = formatBlock opts level $ funcDeclBody funcDecl
      visPrefix = if vis == Public then formatVisibility vis <> " " else ""
   in visPrefix <> "fn " <> name <> params <> " -> " <> retType <> " " <> body

-- | Format parameter list
formatParams :: FormatOptions -> [Parameter] -> Text
formatParams _ params =
  "(" <> T.intercalate ", " (map formatParam params) <> ")"
  where
    formatParam (Parameter name ty) =
      unVarName name <> ": " <> formatType (qualType ty)

-- | Format qualified type
formatQualifiedType :: QualifiedType -> Text
formatQualifiedType (QualifiedType constness ty) =
  let typeStr = formatType ty
   in case constness of
        Const -> "const " <> typeStr
        Mutable -> typeStr

-- | Format type
formatType :: Type -> Text
formatType = \case
  TypePrimitive prim -> formatPrimitiveType prim
  TypeArray (ArrayType ty) -> "[" <> formatQualifiedType ty <> "]"
  TypeNamed name -> unTypeName name
  TypeStruct name -> unTypeName name
  TypeFunction _ -> "fn(...)" -- Simplified
  TypeResult _ -> "result" -- Simplified

-- | Format primitive type
formatPrimitiveType :: PrimitiveType -> Text
formatPrimitiveType = \case
  PrimInt _ -> "int"
  PrimFloat _ -> "float"
  PrimBool -> "bool"
  PrimString -> "str"
  PrimNone -> "none"

-- | Format block
formatBlock :: FormatOptions -> Int -> Block ann -> Text
formatBlock opts level (Block _ stmts) =
  if null stmts
    then "{}"
    else
      let indent = indentation opts level
          innerIndent = indentation opts (level + 1)
          formattedStmts = map (formatStmt opts (level + 1) . unLocated) stmts
       in "{\n"
            <> T.intercalate "\n" (map (innerIndent <>) formattedStmts)
            <> "\n"
            <> indent
            <> "}"

-- | Format statement
formatStmt :: FormatOptions -> Int -> Stmt ann -> Text
formatStmt opts level = \case
  StmtVarDecl name ty maybeInit ->
    let n = unLocated name
        t = formatQualifiedType $ unLocated ty
     in case maybeInit of
          Nothing -> unVarName n <> ": " <> t <> ";"
          Just initExpr -> unVarName n <> ": " <> t <> " = " <> formatExpr opts 0 (unLocated initExpr) <> ";"
  StmtAssign lval op expr ->
    formatLValue opts (unLocated lval)
      <> " "
      <> formatAssignOp op
      <> " "
      <> formatExpr opts 0 (unLocated expr)
      <> ";"
  StmtExpr expr -> formatExpr opts 0 (unLocated expr) <> ";"
  StmtReturn maybeExpr ->
    case maybeExpr of
      Nothing -> "return;"
      Just expr -> "return " <> formatExpr opts 0 (unLocated expr) <> ";"
  StmtIf cond thenBlock maybeElse ->
    let ifPart = "if " <> formatExpr opts 0 (unLocated cond) <> " " <> formatBlock opts level thenBlock
        elsePart = case maybeElse of
          Nothing -> ""
          Just elseBlock -> " else " <> formatBlock opts level elseBlock
     in ifPart <> elsePart
  StmtFor maybeInit maybeCond maybeUpdate body ->
    formatForStmt opts level maybeInit maybeCond maybeUpdate body
  StmtWhile cond body ->
    "while " <> formatExpr opts 0 (unLocated cond) <> " " <> formatBlock opts level body
  StmtBreak -> "break;"
  StmtContinue -> "continue;"
  StmtBlock block -> formatBlock opts level block

-- | Format if statement
-- | Format for statement
formatForStmt ::
  FormatOptions ->
  Int ->
  Maybe (ForInit ann) ->
  Maybe (Located (Expr ann)) ->
  Maybe (Located (Stmt ann)) ->
  Block ann ->
  Text
formatForStmt opts level maybeInit maybeCond maybeUpdate body =
  let initPart = case maybeInit of
        Nothing -> ""
        Just forInit -> formatForInit opts forInit
      condPart = case maybeCond of
        Nothing -> ""
        Just cond -> formatExpr opts 0 (unLocated cond)
      updatePart = case maybeUpdate of
        Nothing -> ""
        Just update -> T.dropEnd 1 $ formatStmt opts level (unLocated update) -- Remove trailing ;
   in "for "
        <> initPart
        <> " "
        <> condPart
        <> "; "
        <> updatePart
        <> " "
        <> formatBlock opts level body

-- | Format for initializer
formatForInit :: FormatOptions -> ForInit ann -> Text
formatForInit opts = \case
  ForInitDecl name ty expr ->
    unVarName (unLocated name)
      <> ": "
      <> formatQualifiedType (unLocated ty)
      <> " = "
      <> formatExpr opts 0 (unLocated expr)
      <> ";"
  ForInitExpr expr ->
    formatExpr opts 0 (unLocated expr) <> ";"

-- | Format expression
formatExpr :: FormatOptions -> Int -> Expr ann -> Text
formatExpr opts prec = \case
  ExprLiteral lit -> formatLiteral lit
  ExprVar name -> unVarName $ unLocated name
  ExprBinary op left right ->
    let leftExpr = formatExpr opts (precedence op) (unLocated left)
        rightExpr = formatExpr opts (precedence op + 1) (unLocated right)
        expr = leftExpr <> " " <> formatBinaryOp op <> " " <> rightExpr
     in if precedence op < prec then "(" <> expr <> ")" else expr
  ExprUnary op operand ->
    formatUnaryOp op <> formatExpr opts 12 (unLocated operand)
  ExprCall func args ->
    unFuncName (unLocated func) <> "(" <> T.intercalate ", " (map (formatExpr opts 0 . unLocated) args) <> ")"
  ExprIndex arr idx ->
    formatExpr opts 13 (unLocated arr) <> "[" <> formatExpr opts 0 (unLocated idx) <> "]"
  ExprArrayInit _ elements ->
    "[" <> T.intercalate ", " (map (formatExpr opts 0 . unLocated) elements) <> "]"
  ExprStructInit name fields ->
    unTypeName (unLocated name) <> " { " <> T.intercalate ", " (map formatField fields) <> " }"
    where
      formatField (fName, fExpr) =
        unFieldName (unLocated fName) <> ": " <> formatExpr opts 0 (unLocated fExpr)
  ExprField expr field ->
    formatExpr opts 13 (unLocated expr) <> "." <> unFieldName (unLocated field)
  ExprTry expr ->
    formatExpr opts 13 (unLocated expr) <> "?"
  ExprMust expr ->
    formatExpr opts 13 (unLocated expr) <> "!"
  ExprParen expr ->
    "(" <> formatExpr opts 0 (unLocated expr) <> ")"
  ExprCast expr ty ->
    formatExpr opts 13 (unLocated expr) <> " as " <> formatType (unLocated ty)

-- | Format LValue
formatLValue :: FormatOptions -> LValue ann -> Text
formatLValue opts = \case
  LVarRef name -> unVarName $ unLocated name
  LArrayIndex arr idx ->
    formatLValue opts (unLocated arr) <> "[" <> formatExpr opts 0 (unLocated idx) <> "]"
  LFieldAccess lval field ->
    formatLValue opts (unLocated lval) <> "." <> unFieldName (unLocated field)

-- | Format literal
formatLiteral :: Literal (Located (Expr ann)) -> Text
formatLiteral = T.pack . show

-- | Format binary operator
formatBinaryOp :: BinaryOp -> Text
formatBinaryOp = binaryOpSymbol

-- | Format unary operator
formatUnaryOp :: UnaryOp -> Text
formatUnaryOp = unaryOpSymbol

-- | Format assignment operator
formatAssignOp :: AssignOp -> Text
formatAssignOp = assignOpSymbol

-- | Get operator precedence
precedence :: BinaryOp -> Int
precedence = binaryOpPrecedence

-- | Format import declaration
formatImportDecl :: ImportDecl -> Text
formatImportDecl (ImportDecl (ModulePath parts) target) =
  let path = T.intercalate "." $ map (unModuleName . unLocated) parts
   in case target of
        ImportAll -> "import " <> path <> ";"
        ImportNames names ->
          "from " <> path <> " import " <> T.intercalate ", " (map (unVarName . unLocated) names) <> ";"
        ImportWildcard -> "from " <> path <> " import *;"

-- | Format struct declaration
formatStructDecl :: FormatOptions -> Int -> Visibility -> StructDecl -> Text
formatStructDecl opts level vis (StructDecl name fields) =
  let indent = indentation opts level
      innerIndent = indentation opts (level + 1)
      visPrefix = if vis == Public then formatVisibility vis <> " " else ""
      formattedFields = map (formatStructField innerIndent) fields
   in visPrefix
        <> "struct "
        <> unTypeName (unLocated name)
        <> " {\n"
        <> T.intercalate ",\n" formattedFields
        <> "\n"
        <> indent
        <> "}"
  where
    formatStructField ind (Located _ (StructField fName fType)) =
      ind <> unFieldName fName <> ": " <> formatType (qualType fType)

-- | Format error declaration
formatErrorDecl :: FormatOptions -> Int -> Visibility -> ErrorDecl -> Text
formatErrorDecl opts level vis (ErrorDecl name fields) =
  let visPrefix = if vis == Public then formatVisibility vis <> " " else ""
   in if null fields
        then visPrefix <> "error " <> unErrorName (unLocated name) <> ";"
        else
          let indent = indentation opts level
              innerIndent = indentation opts (level + 1)
              formattedFields = map (formatErrorField innerIndent) fields
           in visPrefix
                <> "error "
                <> unErrorName (unLocated name)
                <> " {\n"
                <> T.intercalate ",\n" formattedFields
                <> "\n"
                <> indent
                <> "}"
  where
    formatErrorField ind (Located _ (ErrorField fName fType)) =
      ind <> unFieldName fName <> ": " <> formatType fType

-- | Format error set declaration
formatErrorSetDecl :: FormatOptions -> Int -> Visibility -> ErrorSetDecl -> Text
formatErrorSetDecl opts level vis (ErrorSetDecl name members) =
  let indent = indentation opts level
      innerIndent = indentation opts (level + 1)
      visPrefix = if vis == Public then formatVisibility vis <> " " else ""
      formattedMembers = map (formatErrorSetMember innerIndent) members
   in visPrefix
        <> "error_set "
        <> unErrorName (unLocated name)
        <> " {\n"
        <> T.intercalate ",\n" formattedMembers
        <> "\n"
        <> indent
        <> "}"
  where
    formatErrorSetMember ind (Located _ member) =
      ind <> case member of
        ErrorMemberSingle errName -> unErrorName errName
        ErrorMemberSet errName -> unErrorName errName

-- | Generate indentation string
indentation :: FormatOptions -> Int -> Text
indentation opts level =
  T.replicate (level * formatIndentSize opts) " "
