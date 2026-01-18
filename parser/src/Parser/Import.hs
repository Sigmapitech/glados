{-# LANGUAGE OverloadedStrings #-}

module Parser.Import
  ( parseImportDecl,
    parseImportTarget,
    parseModulePath,
  )
where

import AST.Types.AST
  ( ImportDecl (..),
    ImportTarget (..),
    ModulePath (..),
  )
import AST.Types.Common (Located (..), ModuleName (..), VarName (..), getSpan)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchKeyword,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseModulePath :: TokenParser (Located ModulePath)
parseModulePath = do
  allModules <- MP.sepBy1 parseModuleName (matchSymbol ".")
  let combinedSpan = foldl1 (<>) (map getSpan allModules)
      modulePath = ModulePath allModules
  return $ Located combinedSpan modulePath
  where
    parseModuleName :: TokenParser (Located ModuleName)
    parseModuleName = do
      Located span (TokIdentifier name) <- MP.satisfy isIdentifier
      return $ Located span (ModuleName name)

parseImportTarget :: TokenParser (Located ImportTarget)
parseImportTarget =
  MP.choice
    [ parseImportWildcard,
      parseImportNames
    ]

parseImportWildcard :: TokenParser (Located ImportTarget)
parseImportWildcard = do
  Located span _ <- matchSymbol "*"
  return $ Located span ImportWildcard

parseImportNames :: TokenParser (Located ImportTarget)
parseImportNames = do
  allVars <- MP.sepBy1 parseVarName (matchSymbol ",")
  let combinedSpan = foldl1 (<>) (map getSpan allVars)
  return $ Located combinedSpan (ImportNames allVars)
  where
    parseVarName :: TokenParser (Located VarName)
    parseVarName = do
      Located span (TokIdentifier name) <- MP.satisfy isIdentifier
      return $ Located span (VarName name)

parseImportAllDecl :: TokenParser (Located ImportDecl)
parseImportAllDecl = do
  Located importSpan _ <- matchKeyword "import"
  Located pathSpan modulePath <- parseModulePath

  let combinedSpan = importSpan <> pathSpan
      importDecl =
        ImportDecl
          { importPath = modulePath,
            importTarget = ImportAll
          }
  return $ Located combinedSpan importDecl

parseFromImportDecl :: TokenParser (Located ImportDecl)
parseFromImportDecl = do
  Located fromSpan _ <- matchKeyword "from"
  Located pathSpan modulePath <- parseModulePath
  Located importSpan _ <- matchKeyword "import"
  Located targetSpan target <- parseImportTarget

  let combinedSpan = fromSpan <> pathSpan <> importSpan <> targetSpan
      importDecl =
        ImportDecl
          { importPath = modulePath,
            importTarget = target
          }

  return $ Located combinedSpan importDecl

parseImportDecl :: TokenParser (Located ImportDecl)
parseImportDecl =
  MP.choice
    [ parseImportAllDecl,
      parseFromImportDecl
    ]
