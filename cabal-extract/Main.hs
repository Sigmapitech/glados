{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function ((&))
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

classify :: String -> String -> FilePath -> IO [FilePath]
classify rejectedNamePattern allowedNamePattern path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  case (takeFileName path ++ (if isFile then "." ++ takeExtension path else ""), isDir) of
    (name, _)
      | name =~ rejectedNamePattern -> pure []
      | name =~ allowedNamePattern -> pure [path]
    (_, True) -> findCabalFiles path
    _ -> pure []

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir =
  listDirectory dir >>= \entries ->
    let paths = map (dir </>) entries
     in concat <$> mapM (classify rejectedNamePattern allowedNamePattern) paths
  where
    rejectedNamePattern :: String
    rejectedNamePattern = "^\\..*|^dist-newstyle$"
    allowedNamePattern :: String
    allowedNamePattern = ".*\\.cabal"

computeComponentDeps :: String -> Component -> ComponentInfo
computeComponentDeps pkg = dispatcher
  where
    retrieveLibName lib = case libName lib of
      LMainLibName -> "library"
      LSubLibName ucn -> ucn

    dispatcher :: Component -> ComponentInfo
    dispatcher (CLib lib) = wrap "lib" lib retrieveLibName libBuildInfo
    dispatcher (CExe exe) = wrap "exe" exe exeName buildInfo
    dispatcher (CTest ts) = wrap "test" ts testName testBuildInfo
    dispatcher (CBench bk) = wrap "benchmark" bk benchmarkName benchmarkBuildInfo
    dispatcher (CFLib lib) = wrap "flib" lib foreignLibName foreignLibBuildInfo

    wrap ctype blob nameGetter depGetter =
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
    >>= mapM (readGenericPackageDescription Verbosity.deafening)
    >>= \cabals ->
      BL.putStrLn $
        cabals
          & concatMap (convertToComponents . flattenPackageDescription)
          & buildReverseDependencies
          & removeExternalDeps
          & encode
