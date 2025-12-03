{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

{-
  cabal-extract computes the reverse dependencies of target modules
  at the component level (executable, library, testsuite, ...).

  AST depends on ByteCode
  CodeGen depnds on ByteCode

  changing ByteCode:
  - affects VM
  - affects AST
-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Distribution.Compat.NonEmptySet as NonEmptySet
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Component (Component (..))
import qualified Distribution.Verbosity as Verbosity
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
  )
import System.FilePath
  ( takeExtension,
    takeFileName,
    (</>),
  )
import Text.Regex.TDFA ((=~))

{-
  For each module, a .cabal file describe the package with its inner
  components, that can be in multile types (executable, library, testsuite, ...)
  and have their own dependency tree.
-}
data ComponentInfo = ComponentInfo
  { ciName :: String,
    ciDependencies :: [String],
    ciReverseDependencies :: [String],
    ciType :: String
  }
  deriving (Show)

instance ToJSON ComponentInfo where
  toJSON comp =
    object
      [ "name" .= ciName comp,
        "deps" .= ciDependencies comp,
        "rev-deps" .= ciReverseDependencies comp,
        "type" .= ciType comp
      ]

collectMatchingFilesInTree :: String -> String -> FilePath -> IO [FilePath]
collectMatchingFilesInTree acceptedPattern rejectedPattern path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  case (takeFileName path ++ (if isFile then "." ++ takeExtension path else ""), isDir) of
    (name, _)
      | name =~ acceptedPattern -> pure [path]
      | name =~ rejectedPattern -> pure []
    (_, True) -> findCabalFiles path
    _ -> pure []

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir =
  listDirectory dir >>= \entries ->
    let paths = map (dir </>) entries
     in concat <$> mapM (collectMatchingFilesInTree accept reject) paths
  where
    accept = ".*\\.cabal"
    reject = "^\\..*|^dist-newstyle$"

computeComponentDeps :: String -> Component -> ComponentInfo
computeComponentDeps pkg = dispatch
  where
    retrieveLibName lib = case libName lib of
      LMainLibName -> "library" -- they dont have a proper name
      LSubLibName unqualComponentName -> unqualComponentName

    dispatch :: Component -> ComponentInfo
    dispatch (CLib lib) = buildCInfo "lib" lib (retrieveLibName, libBuildInfo)
    dispatch (CExe exe) = buildCInfo "exe" exe (exeName, buildInfo)
    dispatch (CTest ts) = buildCInfo "test" ts (testName, testBuildInfo)
    dispatch (CBench bk) = buildCInfo "benchmark" bk (benchmarkName, benchmarkBuildInfo)
    dispatch (CFLib lib) = buildCInfo "flib" lib (foreignLibName, foreignLibBuildInfo)

    buildCInfo ctype blob (nameGetter, depGetter) =
      ComponentInfo
        { ciName = pkg ++ ":" ++ unUnqualComponentName (nameGetter blob),
          ciType = ctype,
          ciDependencies = map depToString (targetBuildDepends (depGetter blob)),
          ciReverseDependencies = []
        }

convertToComponents :: PackageDescription -> [ComponentInfo]
convertToComponents pd = map (computeComponentDeps pkg) extractComponents
  where
    pkg = unPackageName (pkgName (package pd))
    extractComponents =
      maybe [] (pure . CLib) (library pd)
        ++ map CLib (subLibraries pd)
        ++ map CExe (executables pd)
        ++ map CTest (testSuites pd)
        ++ map CBench (benchmarks pd)
        ++ map CFLib (foreignLibs pd)

depToString :: Dependency -> String
depToString dep =
  intercalate "," $
    map fmt (NonEmptySet.toList (depLibraries dep))
  where
    depPkg = unPackageName (depPkgName dep)
    fmt LMainLibName = depPkg
    fmt (LSubLibName subname) = depPkg ++ ":" ++ unUnqualComponentName subname

buildReverseDependencies :: [ComponentInfo] -> [ComponentInfo]
buildReverseDependencies comps = map addRevDeps comps
  where
    deps =
      concatMap
        (\comp -> map (\d -> (d, ciName comp)) (ciDependencies comp))
        comps

    appendIfMatches name acc (dep, parent) =
      if dep == name then acc ++ [parent] else acc

    addRevDeps comp =
      comp
        { ciReverseDependencies = foldl (appendIfMatches (ciName comp)) [] deps
        }

removeExternalDeps :: [ComponentInfo] -> [ComponentInfo]
removeExternalDeps = map removeExt
  where
    removeExt comp =
      comp
        { ciDependencies = filter (':' `elem`) (ciDependencies comp)
        }

main :: IO ()
main =
  getCurrentDirectory
    >>= findCabalFiles
    >>= extractFlatDepTree
    >>= BL.putStrLn . encode
  where
    extractFlatDepTree x =
      mapM (readGenericPackageDescription Verbosity.deafening) x
        <&> ( \cabals ->
                cabals
                  & concatMap (convertToComponents . flattenPackageDescription)
                  & buildReverseDependencies
                  & removeExternalDeps
            )
