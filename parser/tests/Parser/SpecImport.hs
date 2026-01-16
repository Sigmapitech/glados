{-# LANGUAGE OverloadedStrings #-}

module Parser.SpecImport (importSpec) where

import AST.Types.AST (ImportDecl (..), ImportTarget (..), ModulePath (..))
import AST.Types.Common (Located (..), ModuleName (..), VarName (..), unLocated)
import Error (ParseError)
import Parser.Import (parseImportDecl, parseImportTarget, parseModulePath)
import Parser.Utils (TokenParser, voidSpann)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, runParser)
import Tokens (Token, TokenContent (..))

-- Helper to run parser
runImportParser :: TokenParser a -> [Token] -> Either (ParseErrorBundle [Token] ParseError) a
runImportParser parser = runParser parser "<test>"

-- Helper to check if result is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- Helper to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Helper to extract Right value
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight called on Left"

-- Helper to create Located tokens
loc :: TokenContent -> Token
loc = Located voidSpann

importSpec :: Spec
importSpec = do
  describe "Parser.Import - parseModulePath" $ do
    it "parses single module name" $ do
      let tokens = [loc (TokIdentifier "math")]
      let result = runImportParser parseModulePath tokens
      result `shouldSatisfy` isRight
      let ModulePath paths = unLocated (fromRight result)
      case paths of
        [path] -> unLocated path `shouldBe` ModuleName "math"
        _ -> fail "Expected single module path"

    it "parses dotted module path" $ do
      let tokens = [loc (TokIdentifier "std"), loc (TokSymbol "."), loc (TokIdentifier "io")]
      let result = runImportParser parseModulePath tokens
      result `shouldSatisfy` isRight
      let ModulePath paths = unLocated (fromRight result)
      case paths of
        [path1, path2] -> do
          unLocated path1 `shouldBe` ModuleName "std"
          unLocated path2 `shouldBe` ModuleName "io"
        _ -> fail "Expected two module paths"

    it "parses multi-part module path" $ do
      let tokens = [loc (TokIdentifier "a"), loc (TokSymbol "."), loc (TokIdentifier "b"), loc (TokSymbol "."), loc (TokIdentifier "c")]
      let result = runImportParser parseModulePath tokens
      result `shouldSatisfy` isRight
      let ModulePath paths = unLocated (fromRight result)
      length paths `shouldBe` 3

  describe "Parser.Import - parseImportTarget" $ do
    it "parses wildcard import" $ do
      let tokens = [loc (TokSymbol "*")]
      let result = runImportParser parseImportTarget tokens
      result `shouldSatisfy` isRight
      unLocated (fromRight result) `shouldBe` ImportWildcard

    it "parses single name import" $ do
      let tokens = [loc (TokIdentifier "sin")]
      let result = runImportParser parseImportTarget tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ImportNames [name] -> unLocated name `shouldBe` VarName "sin"
        _ -> fail "Expected ImportNames with single name"

    it "parses multiple names import" $ do
      let tokens = [loc (TokIdentifier "sin"), loc (TokSymbol ","), loc (TokIdentifier "cos"), loc (TokSymbol ","), loc (TokIdentifier "tan")]
      let result = runImportParser parseImportTarget tokens
      result `shouldSatisfy` isRight
      case unLocated (fromRight result) of
        ImportNames [name1, name2, name3] -> do
          unLocated name1 `shouldBe` VarName "sin"
          unLocated name2 `shouldBe` VarName "cos"
          unLocated name3 `shouldBe` VarName "tan"
        _ -> fail "Expected ImportNames with three names"

  describe "Parser.Import - parseImportDecl" $ do
    it "parses import all declaration" $ do
      let tokens = [loc (TokKeyword "import"), loc (TokIdentifier "math")]
      let result = runImportParser parseImportDecl tokens
      result `shouldSatisfy` isRight
      let importDecl = unLocated (fromRight result)
      importTarget importDecl `shouldBe` ImportAll
      let ModulePath paths = importPath importDecl
      case paths of
        [path] -> unLocated path `shouldBe` ModuleName "math"
        _ -> fail "Expected single module path"

    it "parses import all with dotted path" $ do
      let tokens = [loc (TokKeyword "import"), loc (TokIdentifier "std"), loc (TokSymbol "."), loc (TokIdentifier "io")]
      let result = runImportParser parseImportDecl tokens
      result `shouldSatisfy` isRight
      let importDecl = unLocated (fromRight result)
      importTarget importDecl `shouldBe` ImportAll
      let ModulePath paths = importPath importDecl
      length paths `shouldBe` 2

    it "parses from import wildcard" $ do
      let tokens = [loc (TokKeyword "from"), loc (TokIdentifier "math"), loc (TokKeyword "import"), loc (TokSymbol "*")]
      let result = runImportParser parseImportDecl tokens
      result `shouldSatisfy` isRight
      let importDecl = unLocated (fromRight result)
      importTarget importDecl `shouldBe` ImportWildcard
      let ModulePath paths = importPath importDecl
      case paths of
        [path] -> unLocated path `shouldBe` ModuleName "math"
        _ -> fail "Expected single module path"

    it "parses from import single name" $ do
      let tokens = [loc (TokKeyword "from"), loc (TokIdentifier "math"), loc (TokKeyword "import"), loc (TokIdentifier "sin")]
      let result = runImportParser parseImportDecl tokens
      result `shouldSatisfy` isRight
      let importDecl = unLocated (fromRight result)
      case importTarget importDecl of
        ImportNames [name] -> unLocated name `shouldBe` VarName "sin"
        _ -> fail "Expected ImportNames with single name"

    it "parses from import multiple names" $ do
      let tokens = [loc (TokKeyword "from"), loc (TokIdentifier "math"), loc (TokKeyword "import"), loc (TokIdentifier "sin"), loc (TokSymbol ","), loc (TokIdentifier "cos")]
      let result = runImportParser parseImportDecl tokens
      result `shouldSatisfy` isRight
      let importDecl = unLocated (fromRight result)
      case importTarget importDecl of
        ImportNames [name1, name2] -> do
          unLocated name1 `shouldBe` VarName "sin"
          unLocated name2 `shouldBe` VarName "cos"
        _ -> fail "Expected ImportNames with two names"

    it "parses from import with dotted module path" $ do
      let tokens = [loc (TokKeyword "from"), loc (TokIdentifier "std"), loc (TokSymbol "."), loc (TokIdentifier "io"), loc (TokKeyword "import"), loc (TokIdentifier "print"), loc (TokSymbol ","), loc (TokIdentifier "read")]
      let result = runImportParser parseImportDecl tokens
      result `shouldSatisfy` isRight
      let importDecl = unLocated (fromRight result)
      let ModulePath paths = importPath importDecl
      case paths of
        [path1, path2] -> do
          unLocated path1 `shouldBe` ModuleName "std"
          unLocated path2 `shouldBe` ModuleName "io"
        _ -> fail "Expected two module paths"
      case importTarget importDecl of
        ImportNames [_, _] -> True `shouldBe` True
        _ -> fail "Expected ImportNames with two names"

    it "fails on invalid import syntax" $ do
      let tokens = [loc (TokKeyword "from"), loc (TokIdentifier "math")]
      let result = runImportParser parseImportDecl tokens
      result `shouldSatisfy` isLeft
