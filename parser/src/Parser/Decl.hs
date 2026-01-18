module Parser.Decl where

import AST.Types.AST
  ( Decl (DeclFunction, DeclImport),
    FunctionDecl
      ( FunctionDecl,
        funcDeclBody,
        funcDeclName,
        funcDeclParams,
        funcDeclReturnType
      ),
    Visibility (..),
  )
import AST.Types.Common (FuncName (..), Located (..))
import AST.Types.Type (FunctionType (funcParams, funcReturnType))
import Parser.Import (parseImportDecl)
import Parser.Stmt (parseBlock)
import Parser.Type (parseFunctionType)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchKeyword,
    voidSpann,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseVisibility :: TokenParser (Located Visibility)
parseVisibility = do
  maybeStatic <- MP.optional (matchKeyword "static")
  case maybeStatic of
    Just (Located span _) -> return $ Located span Static
    Nothing -> return $ Located voidSpann Public

parseDeclFunction :: TokenParser (Located (Decl ann))
parseDeclFunction = do
  Located visSpan visibility <- parseVisibility
  Located fnSpan _ <- matchKeyword "fn"
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  Located typeSpan funcType <- parseFunctionType
  Located bodySpan block <- parseBlock

  let combinedSpan = case visibility of
        Static -> visSpan <> fnSpan <> nameSpan <> typeSpan <> bodySpan
        Public -> fnSpan <> nameSpan <> typeSpan <> bodySpan

  let functionDecl =
        FunctionDecl
          { funcDeclName = Located nameSpan (FuncName name),
            funcDeclParams = funcParams funcType,
            funcDeclReturnType = funcReturnType funcType,
            funcDeclBody = block
          }
  return $ Located combinedSpan (DeclFunction visibility functionDecl)

parseDecl :: TokenParser (Located (Decl ann))
parseDecl =
  MP.choice
    [ parseDeclFunction,
      do
        Located span importDecl <- parseImportDecl
        return $ Located span (DeclImport importDecl)
    ]
