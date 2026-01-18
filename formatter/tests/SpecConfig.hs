{-# LANGUAGE OverloadedStrings #-}

module SpecConfig (spec) where

import qualified Data.Text as T
import Formatter
  ( FormatOptions (..),
    defaultFormatOptions,
    formatOptionsCodec,
  )
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
  )
import qualified Toml

spec :: Spec
spec = do
  describe "Config TOML codec" $ do
    describe "formatOptionsCodec" $ do
      it "encodes default options correctly" $ do
        let toml = Toml.encode formatOptionsCodec defaultFormatOptions
        T.unpack toml `shouldContain` "indent_size"
        T.unpack toml `shouldContain` "max_width"
        T.unpack toml `shouldContain` "hard_tabs"
        T.unpack toml `shouldContain` "array_width"
        T.unpack toml `shouldContain` "reorder_imports"
        T.unpack toml `shouldContain` "normalize_comments"

      it "round-trips correctly" $ do
        let encoded = Toml.encode formatOptionsCodec defaultFormatOptions
        case Toml.decode formatOptionsCodec encoded of
          Left err -> expectationFailure $ "Failed to decode: " ++ show err
          Right opts -> do
            formatIndentSize opts `shouldBe` formatIndentSize defaultFormatOptions
            formatMaxLineLength opts `shouldBe` formatMaxLineLength defaultFormatOptions
            formatUseSpaces opts `shouldBe` formatUseSpaces defaultFormatOptions
            formatArrayWidth opts `shouldBe` formatArrayWidth defaultFormatOptions
            formatReorderImports opts `shouldBe` formatReorderImports defaultFormatOptions
            formatNormalizeComments opts `shouldBe` formatNormalizeComments defaultFormatOptions

      it "decodes valid TOML config" $ do
        let tomlStr =
              "indent_size = 2\n\
              \max_width = 120\n\
              \hard_tabs = true\n\
              \array_width = 60\n\
              \reorder_imports = true\n\
              \normalize_comments = false\n"
        case Toml.decode formatOptionsCodec tomlStr of
          Left err -> expectationFailure $ "Failed to decode: " ++ show err
          Right opts -> do
            formatIndentSize opts `shouldBe` 2
            formatMaxLineLength opts `shouldBe` 120
            formatUseSpaces opts `shouldBe` False -- hard_tabs = true means useSpaces = false
            formatArrayWidth opts `shouldBe` 60
            formatReorderImports opts `shouldBe` True
            formatNormalizeComments opts `shouldBe` False

      it "all fields are required" $ do
        let tomlStr = "indent_size = 2\n"
        case Toml.decode formatOptionsCodec tomlStr of
          Left _ -> return () -- Expected to fail
          Right _ -> expectationFailure "Should require all fields"

      it "handles boolean hard_tabs correctly" $ do
        let fullConfig =
              "indent_size = 4\n\
              \max_width = 100\n\
              \array_width = 80\n\
              \reorder_imports = false\n\
              \normalize_comments = false\n"
        let tomlSpaces = fullConfig <> "hard_tabs = false\n"
        case Toml.decode formatOptionsCodec tomlSpaces of
          Left err -> expectationFailure $ "Failed to decode: " ++ show err
          Right opts -> formatUseSpaces opts `shouldBe` True

        let tomlTabs = fullConfig <> "hard_tabs = true\n"
        case Toml.decode formatOptionsCodec tomlTabs of
          Left err -> expectationFailure $ "Failed to decode: " ++ show err
          Right opts -> formatUseSpaces opts `shouldBe` False
